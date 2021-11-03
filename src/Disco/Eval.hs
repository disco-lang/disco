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
-----------------------------------------------------------------------------

module Disco.Eval
       (
         -- * Effects

         DiscoEffects

         -- * Top-level info record and associated lenses

       , TopInfo
       , replModInfo, topEnv, lastFile

         -- * Memory

       , Cell(..), mkCell, Loc, showMemory, garbageCollect
       , allocate, delay, delay', mkValue, mkSimple

         -- * Running things

       , runDisco
       , runTCM, runTCMWith
       , withTopEnv
       , parseDiscoModule
       , typecheckTop

         -- * Loading modules

       , loadDiscoModule
       , loadParsedDiscoModule
       , loadFile
       , addToREPLModule
       , setREPLModule
       , populateCurrentModuleInfo
       , loadDefs

       )
       where

import           Control.Arrow                    ((&&&))
import           Control.Exception                (SomeException, handle)
import           Control.Lens                     (makeLenses, toListOf, view,
                                                   (.~))
import           Control.Monad                    (forM_, void, when)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Bifunctor
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IntSet
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           System.FilePath                  ((-<.>))
import           Text.Printf

import qualified System.Console.Haskeline         as H

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
import           Disco.Compile
import           Disco.Context                    as Ctx
import           Disco.Effects.Fresh
import           Disco.Effects.Input
import           Disco.Effects.LFresh
import           Disco.Effects.Output
import           Disco.Error
import           Disco.Extensions
import           Disco.Interpret.CESK
import           Disco.Module
import           Disco.Names
import           Disco.Parser
import           Disco.Typecheck                  (checkModule)
import           Disco.Typecheck.Util
import           Disco.Types
import           Disco.Value
import           Polysemy
import           Polysemy.Embed
import           Polysemy.Fail
import           Polysemy.Random
import           Polysemy.Reader
import           Polysemy.State
import qualified System.Console.Haskeline         as H
import           System.FilePath                  ((-<.>))
import           Unbound.Generics.LocallyNameless (Name)

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
  { _replModInfo :: ModuleInfo
    -- ^ Info about the top-level module collecting stuff entered at
    --   the REPL.

  , _topEnv      :: Env
    -- ^ Top-level environment mapping names to values (which all
    --   start as indirections to thunks).  Set by 'loadDefs'.
    --   Use it when evaluating with 'withTopEnv'.

  , _lastFile    :: Maybe FilePath
    -- ^ The most recent file which was :loaded by the user.
  }

-- | The initial (empty) record of top-level info.
initTopInfo :: TopInfo
initTopInfo = TopInfo
  { _replModInfo = emptyModuleInfo
  , _topEnv      = emptyCtx
  , _lastFile    = Nothing
  }

makeLenses ''TopInfo

------------------------------------------------------------
-- Running top-level Disco computations
------------------------------------------------------------

-- | Settings for running the 'InputT' monad from @haskeline@.  Just
--   uses the defaults and sets the history file to @.disco_history@.
inputSettings :: H.Settings IO
inputSettings =
  H.defaultSettings
    { H.historyFile = Just ".disco_history"
    }

-- | Run a top-level computation.
runDisco :: (forall r. Members DiscoEffects r => Sem r ()) -> IO ()
runDisco =
  void
    . H.runInputT inputSettings
    . runFinal @(H.InputT IO)
    . embedToFinal
    . runEmbedded @_ @(H.InputT IO) liftIO
    . runOutputSem (embed . putStr) -- Handle Output String via printing to console
    . stateToIO initTopInfo -- Run State TopInfo via an IORef
    . inputToState -- Dispatch Input TopInfo effect via State effect
    . runState emptyMem
    . outputDiscoErrors -- Output any top-level errors
    -- . runOutputSem (embed . putStrLn . unDebug)   -- debugging mode
    . ignoreOutput @Debug -- non-debugging mode: ignore Debug output
    . runLFresh -- Generate locally fresh names
    . runRandomIO -- Generate randomness via IO
    . mapError EvalErr -- Embed runtime errors into top-level error type
    . failToError Panic -- Turn pattern-match failures into a Panic error
    . runReader emptyCtx -- Keep track of current Env

------------------------------------------------------------
-- Environment utilities
------------------------------------------------------------

-- | Run a computation with the top-level environment used as the
--   current local environment.  For example, this is used every time
--   we start evaluating an expression entered at the command line.
withTopEnv :: Member (Input TopInfo) r => Sem (Reader Env ': r) a -> Sem r a
withTopEnv m = do
  e <- inputs (view topEnv)
  runReader e m

-- | XXX
inputTopEnv :: Member (Input TopInfo) r => Sem (Input Env ': r) a -> Sem r a
inputTopEnv m = do
  e <- inputs (view topEnv)
  runInputConst e m

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
runTCM ::
  Member (Error DiscoError) r =>
  Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a ->
  Sem r a
runTCM = runTCMWith emptyCtx M.empty

-- | Run a typechecking computation, providing it with local contexts
--   (initialized to the provided arguments) for variable types and
--   type definitions.
runTCMWith ::
  Member (Error DiscoError) r =>
  TyCtx ->
  TyDefCtx ->
  Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a ->
  Sem r a
runTCMWith tyCtx tyDefCtx =
  mapError TypeCheckErr
    . runFresh
    . runReader @TyDefCtx tyDefCtx
    . runReader @TyCtx tyCtx

-- | Run a typechecking computation in the context of the top-level
--   REPL module, re-throwing a wrapped error if it fails.
typecheckTop
  :: Members '[Input TopInfo, Error DiscoError] r
  => Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a
  -> Sem r a
typecheckTop tcm = do
  tyctx  <- inputs (view (replModInfo . miTys))
  imptyctx <- inputs (toListOf (replModInfo . miImports . traverse . miTys))
  tydefs <- inputs (view (replModInfo . miTydefs))
  imptydefs <- inputs (toListOf (replModInfo . miImports . traverse . miTydefs))
  runTCMWith (tyctx <> mconcat imptyctx) (tydefs <> mconcat imptydefs) tcm

--------------------------------------------------
-- Loading

-- | Standard library modules which should always be in scope.
standardModules :: [String]
standardModules = ["list"]

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
  => Bool -> Resolver -> FilePath -> Sem r ModuleInfo
loadDiscoModule quiet resolver m =
  evalState M.empty $ loadDiscoModule' quiet resolver S.empty m

-- | Like 'loadDiscoModule', but start with an already parsed 'Module'
--   instead of loading a module from disk by name.  Also, check it in
--   a context that includes the current top-level context (unlike a
--   module loaded from disk).  Used for e.g. blocks/modules entered
--   at the REPL prompt.
loadParsedDiscoModule
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO] r
  => Bool -> Resolver -> ModuleName -> Module -> Sem r ModuleInfo
loadParsedDiscoModule quiet resolver name m =
  -- XXX don't use M.empty --- use module map from repl module info in TopInfo?
  evalState M.empty $ loadParsedDiscoModule' quiet REPL resolver S.empty name m

-- | Recursively load a Disco module while keeping track of an extra
--   Map from module names to 'ModuleInfo' records, to avoid loading
--   any imported module more than once. Resolve the module, load and
--   parse it, then call 'loadParsedDiscoModule''.
loadDiscoModule'
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO, State (M.Map ModuleName ModuleInfo)] r
  => Bool -> Resolver -> S.Set ModuleName -> FilePath
  -> Sem r ModuleInfo
loadDiscoModule' quiet resolver inProcess modPath  = do
  (resolvedPath, prov) <- resolveModule resolver modPath
                  >>= maybe (throw $ ModuleNotFound modPath) return
  let name = Named prov modPath
  when (S.member name inProcess) (throw $ CyclicImport name)
  modMap <- get
  case M.lookup name modMap of
    Just mi -> return mi
    Nothing -> do
      when (not quiet) $ outputLn $ "Loading " ++ (modPath -<.> "disco") ++ "..."
      cm <- parseDiscoModule resolvedPath
      loadParsedDiscoModule' quiet Standalone resolver (S.insert name inProcess) name cm

  -- | A list of standard library module names
stdLib :: [ModName]
stdLib = ["list"]

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

  -- Recursively load any modules imported by this one, and build a
  -- map with the results.
  mis <- mapM (loadDiscoModule' (withStdlib resolver) inProcess) mns
  let modImps = M.fromList (map (view miName &&& id) mis)

  -- Get context and type definitions from the REPL, in case we are in REPL mode.
  topImports <- inputs (view (replModInfo . miImports))
  topTyCtx   <- inputs (view (replModInfo . miTys))
  topTyDefns <- inputs (view (replModInfo . miTydefs))

  -- Choose the contexts to use based on mode: if we are loading a
  -- standalone module, we should start it in an empty context.  If we
  -- are loading something entered at the REPL, we need to include any
  -- existing top-level REPL context.
  let importMap = case mode of { Standalone -> modImps; REPL -> topImports <> modImps }
      tyctx   = case mode of { Standalone -> emptyCtx ; REPL -> topTyCtx }
      tydefns = case mode of { Standalone -> M.empty ; REPL -> topTyDefns }

  -- Typecheck (and resolve names in) the module.
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
-- is the tricky bit.  ETA: now that loadDefs is rewritten, maybe it's
-- not tricky anymore.

-- | Add things from the given module to the set of currently loaded
--   things.
addToREPLModule
  :: Members '[Error DiscoError, State TopInfo, Reader Env, Store Cell, Output Debug] r
  => ModuleInfo -> Sem r ()
addToREPLModule mi = do
  curMI <- gets @TopInfo (view replModInfo)
  mi' <- mapError TypeCheckErr $ combineModuleInfo [curMI, mi]
  setREPLModule mi'

-- | Set the given 'ModuleInfo' record as the currently loaded
--   module. This also includes updating the top-level state with new
--   term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setREPLModule
  :: Members '[State TopInfo, Reader Env, Store Cell, Output Debug] r
  => ModuleInfo -> Sem r ()
setREPLModule mi = do
  modify @TopInfo $ replModInfo .~ mi
  populateCurrentModuleInfo

-- | Populate various pieces of the top-level info record (docs, type
--   context, type and term definitions) from the 'ModuleInfo' record
--   corresponding to the currently loaded module, and load all the
--   definitions into the current top-level environment.
populateCurrentModuleInfo ::
  Members '[State TopInfo, Reader Env, Error EvalError, State Mem] r =>
  Sem r ()
populateCurrentModuleInfo = do
  tmds <- gets @TopInfo (view (replModInfo . miTermdefs))
  imptmds <- gets @TopInfo (toListOf (replModInfo . miImports . traverse . miTermdefs))
  let cdefns = Ctx.coerceKeys $ fmap compileDefn tmds
      impcdefns = map (Ctx.coerceKeys . fmap compileDefn) imptmds
  mapM_ (uncurry loadDef) (Ctx.assocs (cdefns <> mconcat impcdefns))

loadDef ::
  Members '[Reader Env, State TopInfo, Error EvalError, State Mem] r =>
  Name Core ->
  Core ->
  Sem r ()
loadDef x body = do
  v <- inputToState . inputTopEnv $ eval body
  modify @TopInfo $ topEnv %~ M.insert x v
