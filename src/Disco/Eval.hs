{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Top-level evaluation utilities.
--
-----------------------------------------------------------------------------

module Disco.Eval
       (
         -- * Effects

         DiscoEffects

         -- * Top-level info record and associated lenses

       , TopInfo
       , topModInfo, topCtx, topDefs, topTyDefs, topEnv, topDocs, extSet, lastFile

         -- * Memory

       , Cell(..), mkCell, Loc, showMemory, garbageCollect
       , allocate, delay, delay', mkValue, mkSimple

         -- * Running things

       , runDisco
       , runTCM, runTCMWith
       , withTopEnv
       , parseDiscoModule
       , typecheckDisco

         -- * Loading modules

       , loadDiscoModule
       , loadParsedDiscoModule
       , loadFile
       , addModule
       , setLoadedModule
       , populateCurrentModuleInfo
       , loadDefs

       )
       where

import           Control.Arrow            ((&&&))
import           Control.Exception        (SomeException, handle)
import           Control.Lens             (makeLenses, view, (.~))
import           Control.Monad            (forM_, void, when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Bifunctor
import           Data.Coerce
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           System.FilePath          ((-<.>))
import           Text.Printf

import qualified System.Console.Haskeline as H

import           Disco.Effects.Error
import           Disco.Effects.Input
import           Disco.Effects.LFresh
import           Disco.Effects.Output
import           Disco.Effects.Store
import           Polysemy
import           Polysemy.Embed
import           Polysemy.Fail
import           Polysemy.Random
import           Polysemy.Reader
import           Polysemy.State

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Context
import           Disco.Effects.Fresh
import           Disco.Error
import           Disco.Extensions
import           Disco.Module
import           Disco.Parser
import           Disco.Typecheck          (checkModule)
import           Disco.Typecheck.Monad
import           Disco.Types
import           Disco.Value

------------------------------------------------------------
-- Top-level effects
------------------------------------------------------------

-- | Append two effect rows.
type family AppendEffects (r :: EffectRow) (s :: EffectRow) :: EffectRow where
  AppendEffects '[] s = s
  AppendEffects (e ': r) s = e ': AppendEffects r s
  -- Didn't seem like this already existed in @polysemy@, though I
  -- might have missed it.  Of course we could also use a polymorphic
  -- version from somewhere --- it is just type-level list append.
  -- However, just manually implementing it here seems easier.

-- | Effects needed at the top level.
type TopEffects = '[Error DiscoError, Input TopInfo, State TopInfo, Output String, Embed IO, Final (H.InputT IO)]

-- | All effects needed for the top level + interpretation.
type DiscoEffects = AppendEffects EvalEffects TopEffects

------------------------------------------------------------
-- Top level info record
------------------------------------------------------------

-- | A record of information about the current top-level environment.
data TopInfo = TopInfo
  { _topModInfo :: ModuleInfo
    -- ^ Info about the top-level currently loaded module.  Due to
    --   import statements this may actually be a combination of info
    --   about multiple physical modules.

  , _topCtx     :: Ctx Term PolyType
    -- ^ Top-level type environment.

  , _topDefs    :: Ctx ATerm Defn
    -- ^ Environment of top-level surface syntax definitions.  Set by
    --   'loadDefs' and by 'let' command at the REPL.

  , _topTyDefs  :: TyDefCtx
    -- ^ Environment of top-level type definitions.

  , _topEnv     :: Env
    -- ^ Top-level environment mapping names to values (which all
    --   start as indirections to thunks).  Set by 'loadDefs'.
    --   Use it when evaluating with 'withTopEnv'.

  , _topDocs    :: Ctx Term Docs
    -- ^ Top-level documentation.

  , _extSet     :: ExtSet
    -- ^ Currently enabled language extensions.

  , _lastFile   :: Maybe FilePath
    -- ^ The most recent file which was :loaded by the user.
  }

-- | The initial (empty) record of top-level info.
initTopInfo :: TopInfo
initTopInfo = TopInfo
  { _topModInfo = emptyModuleInfo
  , _topCtx     = emptyCtx
  , _topDefs    = emptyCtx
  , _topTyDefs  = M.empty
  , _topDocs    = emptyCtx
  , _topEnv     = emptyCtx
  , _extSet     = defaultExts
  , _lastFile   = Nothing
  }

makeLenses ''TopInfo

------------------------------------------------------------
-- Running top-level Disco computations
------------------------------------------------------------

-- | Settings for running the 'InputT' monad from @haskeline@.  Just
--   uses the defaults and sets the history file to @.disco_history@.
inputSettings :: H.Settings IO
inputSettings = H.defaultSettings
  { H.historyFile = Just ".disco_history" }

-- | Run a top-level computation.
runDisco :: (forall r. Members DiscoEffects r => Sem r ()) -> IO ()
runDisco
  = void
  . H.runInputT inputSettings
  . runFinal @(H.InputT IO)
  . embedToFinal
  . runEmbedded @_ @(H.InputT IO) liftIO
  . runOutputSem (embed . putStr)  -- Handle Output String via printing to console
  . stateToIO initTopInfo    -- Run State TopInfo via an IORef
  . inputToState             -- Dispatch Input TopInfo effect via State effect
  . outputDiscoErrors        -- Output any top-level errors
  -- . runOutputSem (embed . putStrLn . unDebug)   -- debugging mode
  . ignoreOutput @Debug      -- non-debugging mode: ignore Debug output
  . runLFresh                -- Generate locally fresh names
  . runRandomIO              -- Generate randomness via IO
  . runStore                 -- Keep track of current memory store
  . mapError EvalErr         -- Embed runtime errors into top-level error type
  . failToError Panic        -- Turn pattern-match failures into a Panic error
  . runReader emptyCtx       -- Keep track of current Env

------------------------------------------------------------
-- Memory/environment utilities
------------------------------------------------------------

-- | Run a computation with the top-level environment used as the
--   current local environment.  For example, this is used every time
--   we start evaluating an expression entered at the command line.
withTopEnv :: Member (Input TopInfo) r => Sem (Reader Env ': r) a -> Sem r a
withTopEnv m = do
  e <- inputs (view topEnv)
  runReader e m

-- | Allocate a new memory cell for the given value, and return its
--   location.
allocate :: Members '[Store Cell, Output Debug] r => Value -> Sem r Loc
allocate v = do
  loc <- new (mkCell v)
  debug $ "allocating " ++ show v ++ " at location " ++ show loc
  return loc

-- | Turn a value into a "simple" value which takes up a constant
--   amount of space: some are OK as they are; for others, we turn
--   them into an indirection and allocate a new memory cell for them.
mkSimple :: Members '[Store Cell, Output Debug] r => Value -> Sem r Value
mkSimple v@VNum{}         = return v
mkSimple v@VUnit{}        = return v
mkSimple v@(VInj _ VUnit) = return v
mkSimple v@VConst{}       = return v
mkSimple v@VClos{}        = return v
mkSimple v@VType{}        = return v
mkSimple v@VIndir{}       = return v
mkSimple v                = VIndir <$> allocate v

-- | Delay a computation by packaging it into a @VDelay@ constructor
--   along with the current environment.
delay :: Members EvalEffects r => (forall r'. Members EvalEffects r' => Sem r' Value) -> Sem r Value
delay = delay' []

-- | Like 'delay', but also specify a set of values which will be
--   needed during the delayed computation, to prevent any memory
--   referenced by the values from being garbage collected.
delay' :: Members EvalEffects r => [Value] -> (forall r'. Members EvalEffects r' => Sem r' Value) -> Sem r Value
delay' vs imv = do
  ls <- getReachable vs
  VDelay imv ls <$> getEnv

-- | Turn a Core expression into a value.  Some kinds of expressions
--   can be turned into corresponding values directly; for others we
--   create a thunk by packaging up the @Core@ expression with the
--   current environment.  The thunk is stored in a new location in
--   memory, and the returned value consists of an indirection
--   referring to its location.
mkValue :: Members '[Reader Env, Store Cell, Output Debug] r => Core -> Sem r Value
mkValue (CConst op)   = return $ VConst op
mkValue CUnit         = return VUnit
mkValue (CInj s v)    = VInj s <$> mkValue v
mkValue (CPair v1 v2) = VPair <$> mkValue v1 <*> mkValue v2
mkValue (CNum d r)    = return $ VNum d r
mkValue (CType ty)    = return $ VType ty
mkValue c             = VIndir <$> (allocate . VThunk c =<< getEnv)

-- | Deallocate any memory cells which are no longer recursively
--   referenced by any top-level binding.
garbageCollect :: Members '[State TopInfo, Store Cell] r => Sem r ()
garbageCollect = do
  env  <- gets @TopInfo (view topEnv)
  keep <- getReachable env
  keepKeys keep

-- | Get the set of memory locations reachable from a set of values.
getReachable :: (Reachable v, Members '[Store Cell] r) => v -> Sem r IntSet
getReachable = execState IntSet.empty . mark

class Reachable v where
  -- | @mark v@ marks the memory locations reachable from the values
  --   stored in @v@.
  mark :: Members '[Store Cell, State IntSet] r => v -> Sem r ()

instance Reachable Value where
  mark (VInj _ v)      = mark v
  mark (VPair v1 v2)   = mark v1 >> mark v2
  mark (VClos _ e)     = mark e
  mark (VPAp v vs)     = mark (v:vs)
  mark (VThunk _ e)    = mark e
  mark (VIndir l)      = mark l
  mark (VDelay _ ls e) = (modify @IntSet $ IntSet.union ls) >> mark e
  mark (VBag vs)       = mark (map fst vs)
  mark (VProp p)       = mark p
  mark (VGraph _ adj)  = mark adj
    -- A graph can only contain SimpleValues, which by def contain no indirection.
    -- However its buffered adjacency map can.
  mark (VMap m)        = mark (M.elems m)
  mark _               = return ()

instance Reachable Env where
  mark = mark . M.elems

instance Reachable v => Reachable [v] where
  mark = mapM_ mark

instance Reachable ValProp where
  mark (VPDone (TestResult _ r vs)) = mapM_ mark r >> mark vs
  mark (VPSearch _ _ v vs)          = mark v >> mark vs

instance Reachable TestEnv where
  mark (TestEnv te) = forM_ te $ \(_, _, v) -> mark v

instance Reachable Loc where
  mark l = do
    reach <- get @IntSet
    case IntSet.member l reach of
      True -> return ()
      False -> do
        modify $ IntSet.insert l
        mc <- lookupStore l
        case mc of
          Nothing         -> return ()
          Just (Cell v _) -> mark v

-- | Show the current contents of memory, for debugging purposes.
showMemory :: Members '[Store Cell, Output String] r => Sem r ()
showMemory = assocsStore >>= mapM_ showCell
  where
    showCell :: Member (Output String) r => (Int, Cell) -> Sem r ()
    showCell (i, Cell v b) = output $ printf "%3d%s %s\n" i (if b then "!" else " ") (show v)

------------------------------------------------------------
-- High-level disco phases
------------------------------------------------------------

--------------------------------------------------
-- Parsing

-- | Parse a module from a file, re-throwing a parse error if it
--   fails.
parseDiscoModule :: Members '[Error DiscoError, Embed IO] r => FilePath -> Sem r Module
parseDiscoModule file = do
  str <- liftIO $ readFile file
  fromEither . first ParseErr $ runParser (wholeModule Standalone) file str

--------------------------------------------------
-- Type checking

-- | Run a typechecking computation, providing it with local
--   (initially empty) contexts for variable types and type
--   definitions.
runTCM
  :: Member (Error DiscoError) r
  => Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a
  -> Sem r a
runTCM = runTCMWith emptyCtx M.empty

-- | Run a typechecking computation, providing it with local contexts
--   (initialized to the provided arguments) for variable types and
--   type definitions.
runTCMWith
  :: Member (Error DiscoError) r
  => TyCtx -> TyDefCtx
  -> Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a
  -> Sem r a
runTCMWith tyCtx tyDefCtx
  = mapError TypeCheckErr
  . runFresh
  . runReader @TyDefCtx tyDefCtx
  . runReader @TyCtx tyCtx

-- | Run a typechecking computation, re-throwing a wrapped error if it
--   fails.
typecheckDisco
  :: Members '[Input TopInfo, Error DiscoError] r
  => Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a
  -> Sem r a
typecheckDisco tcm = do
  tyctx  <- inputs (view topCtx)
  tydefs <- inputs (view topTyDefs)
  runTCMWith tyctx tydefs tcm

--------------------------------------------------
-- Loading

-- | Recursively loads a given module by first recursively loading and
--   typechecking its imported modules, adding the obtained
--   'ModuleInfo' records to a map from module names to info records,
--   and then typechecking the parent module in an environment with
--   access to this map. This is really just a depth-first search.
--
--   The 'Resolver' argument specifies where to look for imported
--   modules.
loadDiscoModule
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO] r
  => Resolver -> FilePath -> Sem r ModuleInfo
loadDiscoModule resolver m =
  evalState M.empty $ loadDiscoModule' resolver S.empty m

-- | Like 'loadDiscoModule', but start with an already parsed 'Module'
--   instead of loading a module from disk by name.  Also, check it in
--   a context that includes the current top-level context (unlike a
--   module loaded from disk).  Used for e.g. blocks/modules entered
--   at the REPL prompt.
loadParsedDiscoModule
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO] r
  => Resolver -> ModuleName -> Module -> Sem r ModuleInfo
loadParsedDiscoModule resolver name m =
  evalState M.empty $ loadParsedDiscoModule' REPL resolver S.empty name m

-- | Recursively load a Disco module while keeping track of an extra
--   Map from module names to 'ModuleInfo' records, to avoid loading
--   any imported module more than once. Resolve the module, load and
--   parse it, then call 'loadParsedDiscoModule''.
loadDiscoModule'
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO, State (M.Map ModuleName ModuleInfo)] r
  => Resolver -> S.Set ModuleName -> FilePath
  -> Sem r ModuleInfo
loadDiscoModule' resolver inProcess modPath  = do
  (resolvedPath, prov) <- resolveModule resolver modPath
                  >>= maybe (throw $ ModuleNotFound modPath) return
  let name = Named prov modPath
  when (S.member name inProcess) (throw $ CyclicImport name)
  modMap <- get
  case M.lookup name modMap of
    Just mi -> return mi
    Nothing -> do
      outputLn $ "Loading " ++ (modPath -<.> "disco") ++ "..."
      cm <- parseDiscoModule resolvedPath
      loadParsedDiscoModule' Standalone resolver (S.insert name inProcess) name cm

-- | Recursively load an already-parsed Disco module while keeping
--   track of an extra Map from module names to 'ModuleInfo' records,
--   to avoid loading any imported module more than once.  Typecheck
--   it in the context of the top-level type context iff the
--   'LoadingMode' parameter is 'REPL'.  Recursively load all its
--   imports, then typecheck it.
loadParsedDiscoModule'
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO, State (M.Map ModuleName ModuleInfo)] r
  => LoadingMode -> Resolver -> S.Set ModuleName -> ModuleName -> Module -> Sem r ModuleInfo
loadParsedDiscoModule' mode resolver inProcess name cm@(Module _ mns _ _ _) = do
  mis <- mapM (loadDiscoModule' (withStdlib resolver) inProcess) mns
  let importMap = M.fromList $ map (view miName &&& id) mis
  topTyCtx   <- inputs (view topCtx)
  topTyDefns <- inputs (view topTyDefs)
  let tyctx   = case mode of { Standalone -> emptyCtx ; REPL -> topTyCtx }
  let tydefns = case mode of { Standalone -> M.empty ; REPL -> topTyDefns }
  -- XXX NOTE, checkModule will now need to add stuff from imports to its context!
  m  <- runTCMWith tyctx tydefns $ checkModule name importMap cm
  modify (M.insert name m)
  return m

-- | Try loading the contents of a file from the filesystem, emitting
--   an error if it's not found.
loadFile :: Members '[Output String, Embed IO] r => FilePath -> Sem r (Maybe String)
loadFile file = do
  res <- liftIO $ handle @SomeException (return . Left) (Right <$> readFile file)
  case res of
    Left _  -> outputLn ("File not found: " ++ file) >> return Nothing
    Right s -> return (Just s)

-- XXX This is recompiling + re-adding everything every time a new
-- module is added.  Can we do this more efficiently?  Only add new
-- stuff incrementally.  But we still have to check there are no
-- conflicts?  Or do we?  Actually, maybe not!  New stuff when being
-- entered as a block should override/shadow anything previous.  Need
-- a new function that does something similar to
-- populateCurrentmoduleinfo but adds instead of completely replaces
-- (but does in fact override anything with the same name).  loadDefs
-- is the tricky bit.

-- | Add things from the given module to the set of currently loaded
--   things.
addModule
  :: Members '[Error DiscoError, State TopInfo, Reader Env, Store Cell, Output Debug] r
  => ModuleInfo -> Sem r ()
addModule mi = do
  curMI <- gets @TopInfo (view topModInfo)
  mi' <- mapError TypeCheckErr $ combineModuleInfo [curMI, mi]
  setLoadedModule mi'

-- | Set the given 'ModuleInfo' record as the currently loaded
--   module. This also includes updating the top-level state with new
--   term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule
  :: Members '[State TopInfo, Reader Env, Store Cell, Output Debug] r
  => ModuleInfo -> Sem r ()
setLoadedModule mi = do
  modify @TopInfo $ topModInfo .~ mi
  populateCurrentModuleInfo

-- | Populate various pieces of the top-level info record (docs, type
--   context, type and term definitions) from the 'ModuleInfo' record
--   corresponding to the currently loaded module, and load all the
--   definitions into the current top-level environment.
populateCurrentModuleInfo
  :: Members '[State TopInfo, Reader Env, Store Cell, Output Debug] r
  => Sem r ()
populateCurrentModuleInfo = do
  ModuleInfo _ _ docs _ tys tyds tmds _ _ <- gets @TopInfo (view topModInfo)
  let cdefns = M.mapKeys coerce $ fmap compileDefn tmds
  modify @TopInfo $
    (topDocs   .~ docs) .
    (topCtx    .~ tys)  .
    (topTyDefs .~ tyds) .
    (topDefs   .~ tmds)
  loadDefs cdefns

-- | Load a top-level environment of (potentially recursive)
--   core language definitions into memory.
loadDefs
  :: Members '[Reader Env, Store Cell, State TopInfo, Output Debug] r
  => Ctx Core Core -> Sem r ()
loadDefs cenv = do

  -- Clear out any leftover memory.
  clearStore

  -- Take the environment mapping names to definitions, and turn
  -- each one into an indirection to a thunk stored in memory.
  env <- traverse mkValue cenv

  -- Top-level definitions are allowed to be recursive, so each
  -- one of those thunks should actually have the environment
  -- 'env' as its environment, so all the top-level definitions
  -- can mutually refer to each other.
  --
  -- For now we know that the only things we have stored in memory
  -- are the thunks we just made, so just iterate through them and
  -- replace their environments.
  mapStore (replaceThunkEnv env)

  -- Finally, set the top-level environment to the one we just
  -- created.
  modify @TopInfo (topEnv .~ env)

  where
    replaceThunkEnv e (Cell (VThunk c _) b) = Cell (VThunk c e) b
    replaceThunkEnv _ c                     = c
