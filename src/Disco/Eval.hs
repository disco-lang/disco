{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Disco.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Top-level evaluation utilities.
module Disco.Eval
  ( -- * Effects
    DiscoEffects,

    -- * Top-level info record and associated lenses
    TopInfo,
    topModInfo,
    topCtx,
    topDefs,
    topTyDefs,
    topEnv,
    topDocs,
    extSet,
    lastFile,

    -- * Running things
    runDisco,
    runTCM,
    runTCMWith,
    withTopEnv,
    inputTopEnv,
    parseDiscoModule,
    typecheckDisco,

    -- * Loading modules
    loadDiscoModule,
    loadParsedDiscoModule,
    loadFile,
    addModule,
    setLoadedModule,
    populateCurrentModuleInfo,
    loadDef,
  )
where

import Control.Exception (SomeException, handle)
import Control.Lens (makeLenses, view, (%~), (.~))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import Disco.AST.Core
import Disco.AST.Surface
import Disco.AST.Typed
import Disco.Compile
import Disco.Context
import Disco.Effects.Error
import Disco.Effects.Fresh
import Disco.Effects.Input
import Disco.Effects.LFresh
import Disco.Effects.Output
import Disco.Error
import Disco.Extensions
import Disco.Interpret.CESK
import Disco.Module
import Disco.Parser
import Disco.Typecheck (checkModule)
import Disco.Typecheck.Monad
import Disco.Types
import Disco.Value
import Polysemy
import Polysemy.Embed
import Polysemy.Fail
import Polysemy.Random
import Polysemy.Reader
import Polysemy.State
import qualified System.Console.Haskeline as H
import System.FilePath ((-<.>))
import Unbound.Generics.LocallyNameless (Name)

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
  { -- | Info about the top-level currently loaded module.  Due to
    --   import statements this may actually be a combination of info
    --   about multiple physical modules.
    _topModInfo :: ModuleInfo,
    -- | All the modules which have been loaded so far.
    _topModMap :: M.Map ModName ModuleInfo,
    -- | Top-level type environment.
    _topCtx :: Ctx Term PolyType,
    -- | Environment of top-level surface syntax definitions.  Set by
    --   'loadDef' and by 'let' command at the REPL.
    _topDefs :: Ctx ATerm Defn,
    -- | Environment of top-level type definitions.
    _topTyDefs :: TyDefCtx,
    -- | Top-level environment mapping names to values (which all
    --   start as indirections to thunks).  Set by 'loadDef'.
    --   Use it when evaluating with 'withTopEnv'.
    _topEnv :: Env,
    -- | Top-level documentation.
    _topDocs :: Ctx Term Docs,
    -- | Currently enabled language extensions.
    _extSet :: ExtSet,
    -- | The most recent file which was :loaded by the user.
    _lastFile :: Maybe FilePath
  }

-- | The initial (empty) record of top-level info.
initTopInfo :: TopInfo
initTopInfo =
  TopInfo
    { _topModInfo = emptyModuleInfo,
      _topModMap = M.empty,
      _topCtx = emptyCtx,
      _topDefs = emptyCtx,
      _topTyDefs = M.empty,
      _topDocs = emptyCtx,
      _topEnv = emptyCtx,
      _extSet = defaultExts,
      _lastFile = Nothing
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

-- | Run a typechecking computation, re-throwing a wrapped error if it
--   fails.
typecheckDisco ::
  Members '[Input TopInfo, Error DiscoError] r =>
  Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a ->
  Sem r a
typecheckDisco tcm = do
  tyctx <- inputs (view topCtx)
  tydefs <- inputs (view topTyDefs)
  runTCMWith tyctx tydefs tcm

--------------------------------------------------
-- Loading

-- XXX NEXT: load standard module(s) and add them to the Map before
-- loading other things

-- XXX should we save the map at the top level, so we don't have to
-- keep reloading things every time?

-- | Recursively loads a given module by first recursively loading and
--   typechecking its imported modules, adding the obtained
--   'ModuleInfo' records to a map from module names to info records,
--   and then typechecking the parent module in an environment with
--   access to this map. This is really just a depth-first search.
--
--   The 'Resolver' argument specifies where to look for imported
--   modules.
loadDiscoModule ::
  Members '[State TopInfo, Output String, Error DiscoError, Embed IO] r =>
  Bool ->
  Resolver ->
  ModName ->
  Sem r ModuleInfo
loadDiscoModule quiet resolver m = do
  mods <- gets @TopInfo (view topModMap)
  (mods', mi) <- inputToState . runState mods $ loadDiscoModule' quiet resolver S.empty m
  modify @TopInfo (topModMap .~ mods')
  return mi

-- | Like 'loadDiscoModule', but start with an already parsed 'Module'
--   instead of loading a module from disk by name.  Also, check it in
--   a context that includes the current top-level context (unlike a
--   module loaded from disk).  Used for e.g. blocks/modules entered
--   at the REPL prompt.
loadParsedDiscoModule ::
  Members '[State TopInfo, Output String, Error DiscoError, Embed IO] r =>
  Bool ->
  Resolver ->
  ModName ->
  Module ->
  Sem r ModuleInfo
loadParsedDiscoModule quiet resolver modName m = do
  mods <- gets @TopInfo (view topModMap)
  (mods', mi) <- inputToState . runState mods $ loadParsedDiscoModule' quiet REPL resolver S.empty modName m
  modify @TopInfo (topModMap .~ mods')
  return mi

-- | Recursively load a Disco module while keeping track of an extra
--   Map from module names to 'ModuleInfo' records, to avoid loading
--   any imported module more than once. Resolve the module, load and
--   parse it, then call 'loadParsedDiscoModule''.
loadDiscoModule' ::
  Members '[Input TopInfo, Output String, Error DiscoError, Embed IO, State (M.Map ModName ModuleInfo)] r =>
  Bool ->
  Resolver ->
  S.Set ModName ->
  ModName ->
  Sem r ModuleInfo
loadDiscoModule' quiet resolver inProcess modName = do
  when (S.member modName inProcess) (throw $ CyclicImport modName)
  modMap <- get
  case M.lookup modName modMap of
    Just mi -> return mi
    Nothing -> do
      file <-
        resolveModule resolver modName
          >>= maybe (throw $ ModuleNotFound modName) return
      when (not quiet) $ outputLn $ "Loading " ++ (modName -<.> "disco") ++ "..."
      cm <- parseDiscoModule file
      loadParsedDiscoModule' quiet Standalone resolver (S.insert modName inProcess) modName cm

-- | Recursively load an already-parsed Disco module while keeping
--   track of an extra Map from module names to 'ModuleInfo' records,
--   to avoid loading any imported module more than once.  Typecheck
--   it in the context of the top-level type context iff the
--   'LoadingMode' parameter is 'REPL'.  Recursively load all its
--   imports, then typecheck it.
loadParsedDiscoModule' ::
  Members '[Input TopInfo, Output String, Error DiscoError, Embed IO, State (M.Map ModName ModuleInfo)] r =>
  Bool ->
  LoadingMode ->
  Resolver ->
  S.Set ModName ->
  ModName ->
  Module ->
  Sem r ModuleInfo
loadParsedDiscoModule' quiet mode resolver inProcess modName cm@(Module _ mns _ _ _) = do
  mis <- mapM (loadDiscoModule' quiet (withStdlib resolver) inProcess) mns
  imports@(ModuleInfo _ _ tyctx tydefns _ _ _) <- mapError TypeCheckErr $ combineModuleInfo Standalone mis
  topTyCtx <- inputs (view topCtx)
  topTyDefns <- inputs (view topTyDefs)
  let tyctx' = case mode of Standalone -> tyctx; REPL -> joinCtx topTyCtx tyctx
  let tydefns' = case mode of Standalone -> tydefns; REPL -> M.union topTyDefns tydefns
  m <- runTCMWith tyctx' tydefns' $ checkModule cm
  m' <- mapError TypeCheckErr $ combineModuleInfo mode [imports, m]
  modify (M.insert modName m')
  return m'

-- | Try loading the contents of a file from the filesystem, emitting
--   an error if it's not found.
loadFile :: Members '[Output String, Embed IO] r => FilePath -> Sem r (Maybe String)
loadFile file = do
  res <- liftIO $ handle @SomeException (return . Left) (Right <$> readFile file)
  case res of
    Left _ -> outputLn ("File not found: " ++ file) >> return Nothing
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
addModule ::
  Members '[Error DiscoError, State TopInfo, Reader Env, Error EvalError, State Mem] r =>
  LoadingMode ->
  ModuleInfo ->
  Sem r ()
addModule mode mi = do
  curMI <- gets @TopInfo (view topModInfo)
  mi' <- mapError TypeCheckErr $ combineModuleInfo mode [curMI, mi]
  setLoadedModule mi'

-- | Set the given 'ModuleInfo' record as the currently loaded
--   module. This also includes updating the top-level state with new
--   term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule ::
  Members '[State TopInfo, Reader Env, Error EvalError, State Mem] r =>
  ModuleInfo ->
  Sem r ()
setLoadedModule mi = do
  modify @TopInfo $ topModInfo .~ mi
  populateCurrentModuleInfo

-- | Populate various pieces of the top-level info record (docs, type
--   context, type and term definitions) from the 'ModuleInfo' record
--   corresponding to the currently loaded module, and load all the
--   definitions into the current top-level environment.
populateCurrentModuleInfo ::
  Members '[State TopInfo, Reader Env, Error EvalError, State Mem] r =>
  Sem r ()
populateCurrentModuleInfo = do
  ModuleInfo docs _ tys tyds tmds _ _ <- gets @TopInfo (view topModInfo)
  let cdefns = compileDefns tmds
  modify @TopInfo $
    (topDocs .~ docs)
      . (topCtx .~ tys)
      . (topTyDefs .~ tyds)
      . (topDefs .~ tmds)
  mapM_ (uncurry loadDef) cdefns

loadDef ::
  Members '[Reader Env, State TopInfo, Error EvalError, State Mem] r =>
  Name Core ->
  Core ->
  Sem r ()
loadDef x body = do
  v <- inputToState . inputTopEnv $ eval body
  modify @TopInfo $ topEnv %~ M.insert x v

-- -- | Load a top-level environment of (potentially recursive)
-- --   core language definitions into memory.
-- loadDefs
--   :: Members '[Reader Env, State TopInfo, Error EvalError, State Mem] r
--   => Ctx Core Core -> Sem r ()
-- loadDefs defs = do

--   let vars = M.keysSet defs

--       -- Get a list of pairs of the form (y,x) where x uses y in its definition.
--       -- We want them in the order (y,x) since y needs to be evaluated before x.
--       -- These will be the edges in our dependency graph.
--       deps = S.unions . map (\(x,body) -> S.map (,x) (setOf fv body)) . M.assocs $ defs

--       -- Do a topological sort of the condensation of the dependency
--       -- graph.  Each SCC corresponds to a group of mutually recursive
--       -- definitions; each such group depends only on groups that come
--       -- before it in the topsort.
--       defnGroups :: [Set (Name Core)]
--       defnGroups = G.topsort (G.condensation (G.mkGraph vars deps))

--   -- Load the definition groups one by one in order of the topsort.
--   mapM_ (loadDefGroup . M.restrictKeys defs) defnGroups

-- -- | Load a mutually recursive group of definitions, adding them to
-- --   the 'topEnv'.
-- loadDefGroup
--   :: Members '[Reader Env, State TopInfo, Error EvalError, State Mem] r
--   => Ctx Core Core -> Sem r ()
-- loadDefGroup defs = do
--   -- XXX For mutually recursive groups, we need some special support here!
--   newEnv <- inputToState . inputTopEnv $ mapM eval defs
--   modify @TopInfo $ topEnv %~ joinCtx newEnv
