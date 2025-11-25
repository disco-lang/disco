{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Disco.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Top-level evaluation utilities.
module Disco.Eval (
  -- * Effects
  EvalEffects,
  DiscoEffects,

  -- * Top-level info record and associated lenses
  DiscoConfig,
  initDiscoConfig,
  debugMode,
  TopInfo,
  replModInfo,
  topEnv,
  topModMap,
  lastFile,
  discoConfig,

  -- * Running things
  runDisco,
  runTCM,
  inputTopEnv,
  parseDiscoModule,
  typecheckTop,

  -- * Loading modules
  loadDiscoModule,
  loadParsedDiscoModule,
  loadFile,
  addToREPLModule,
  setREPLModule,
  loadDefsFrom,
  loadDef,
)
where

import Control.Arrow ((&&&))
import Control.Exception (SomeException, handle)
import Control.Lens (
  makeLenses,
  toListOf,
  view,
  (%~),
  (.~),
  (<>~),
  (^.),
 )
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import System.FilePath ((-<.>))
import Prelude

import qualified System.Console.Haskeline as H

import Disco.Effects.Fresh
import Disco.Effects.Input
import Disco.Effects.LFresh
import Disco.Effects.State
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Output
import Polysemy.Random
import Polysemy.Reader

import qualified Data.List.NonEmpty as NonEmpty
import Disco.AST.Core
import Disco.AST.Surface
import Disco.Compile (compileDefns)
import Disco.Context as Ctx
import Disco.Error
import Disco.Exhaustiveness (checkClauses)
import Disco.Extensions
import Disco.Interpret.CESK
import Disco.Messages
import Disco.Module
import Disco.Names
import Disco.Parser
import Disco.Pretty hiding ((<>))
import qualified Disco.Pretty as Pretty
import Disco.Typecheck (checkModule)
import Disco.Typecheck.Util
import Disco.Types
import Disco.Value

------------------------------------------------------------
-- Configuation options
------------------------------------------------------------

newtype DiscoConfig = DiscoConfig
  { _debugMode :: Bool
  }

makeLenses ''DiscoConfig

initDiscoConfig :: DiscoConfig
initDiscoConfig =
  DiscoConfig
    { _debugMode = False
    }

------------------------------------------------------------
-- Top level info record
------------------------------------------------------------

-- | A record of information about the current top-level environment.
data TopInfo = TopInfo
  { _replModInfo :: ModuleInfo
  -- ^ Info about the top-level module collecting stuff entered at
  --   the REPL.
  , _topEnv :: Env
  -- ^ Top-level environment mapping names to values.  Set by
  --   'loadDefs'.
  , _topModMap :: Map ModuleName ModuleInfo
  -- ^ Mapping from loaded module names to their 'ModuleInfo'
  --   records.
  , _lastFile :: Maybe FilePath
  -- ^ The most recent file which was :loaded by the user.
  , _discoConfig :: DiscoConfig
  }

-- | The initial (empty) record of top-level info.
initTopInfo :: DiscoConfig -> TopInfo
initTopInfo cfg =
  TopInfo
    { _replModInfo = emptyModuleInfo
    , _topEnv = emptyCtx
    , _topModMap = M.empty
    , _lastFile = Nothing
    , _discoConfig = cfg
    }

makeLenses ''TopInfo

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
type TopEffects = '[Error DiscoError, State TopInfo, Output Message, Embed IO, Final (H.InputT IO)]

-- | Effects needed for evaluation.
type EvalEffects = [Error EvalError, Random, LFresh, Output Message, State Mem]

-- XXX write about order.
-- memory, counter etc. should not be reset by errors.

-- | All effects needed for the top level + evaluation.
type DiscoEffects = AppendEffects EvalEffects TopEffects

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
runDisco :: DiscoConfig -> (forall r. Members DiscoEffects r => Sem r ()) -> IO ()
runDisco cfg =
  void
    . H.runInputT inputSettings
    . runFinal @(H.InputT IO)
    . embedToFinal @(H.InputT IO)
    . runEmbedded @_ @(H.InputT IO) liftIO
    . runOutputSem (handleMsg msgFilter) -- Handle Output Message via printing to console
    . stateToIO (initTopInfo cfg) -- Run State TopInfo via an IORef
    . inputToState @TopInfo -- Dispatch Input TopInfo effect via State effect
    . runState emptyMem -- Start with empty memory
    . outputDiscoErrors -- Output any top-level errors
    . runLFresh -- Generate locally fresh names
    . runRandomIO -- Generate randomness via IO
    . mapError EvalErr -- Embed runtime errors into top-level error type
    . failToError Panic -- Turn pattern-match failures into a Panic error
    . runReader emptyCtx -- Keep track of current Env
 where
  msgFilter
    | cfg ^. debugMode = const True
    | otherwise = (/= Debug) . view messageType

------------------------------------------------------------
-- Environment utilities
------------------------------------------------------------

-- XXX change name to inputREPLEnv, modify to actually get the Env
-- from the REPL module info?

-- | Run a computation that needs an input environment, grabbing the
--   current top-level environment from the 'TopInfo' records.
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

-- | Run a typechecking computation, providing it with local contexts
--   (initialized to the provided arguments) for variable types and
--   type definitions.
runTCM ::
  Member (Error DiscoError) r =>
  TyCtx ->
  TyDefCtx ->
  Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error LocTCError ': r) a ->
  Sem r a
runTCM tyCtx tyDefCtx =
  mapError TypeCheckErr
    . runFresh
    . runReader @TyDefCtx tyDefCtx
    . runReader @TyCtx tyCtx

-- | A variant of 'runTCM' that requires only a 'TCError' instead
--   of a 'LocTCError'.
runTCM' ::
  Member (Error DiscoError) r =>
  TyCtx ->
  TyDefCtx ->
  Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a ->
  Sem r a
runTCM' tyCtx tyDefCtx =
  mapError (TypeCheckErr . noLoc)
    . runFresh
    . runReader @TyDefCtx tyDefCtx
    . runReader @TyCtx tyCtx

-- | Run a typechecking computation in the context of the top-level
--   REPL module, re-throwing a wrapped error if it fails.
typecheckTop ::
  Members '[Input TopInfo, Error DiscoError] r =>
  Sem (Reader TyCtx ': Reader TyDefCtx ': Fresh ': Error TCError ': r) a ->
  Sem r a
typecheckTop tcm = do
  tyctx <- inputs (view (replModInfo . miTys))
  imptyctx <- inputs (toListOf (replModInfo . miImports . traverse . miTys))
  tydefs <- inputs (view (replModInfo . miTydefs))
  imptydefs <- inputs (toListOf (replModInfo . miImports . traverse . miTydefs))
  runTCM' (tyctx <> mconcat imptyctx) (tydefs <> mconcat imptydefs) tcm

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
loadDiscoModule ::
  Members '[State TopInfo, Output Message, Random, State Mem, Error DiscoError, Embed IO] r =>
  Bool ->
  Resolver ->
  FilePath ->
  Sem r ModuleInfo
loadDiscoModule quiet resolver =
  loadDiscoModule' quiet resolver []

-- | Like 'loadDiscoModule', but start with an already parsed 'Module'
--   instead of loading a module from disk by name.  Also, check it in
--   a context that includes the current top-level context (unlike a
--   module loaded from disk).  Used for e.g. blocks/modules entered
--   at the REPL prompt.
loadParsedDiscoModule ::
  Members '[State TopInfo, Output Message, Random, State Mem, Error DiscoError, Embed IO] r =>
  Bool ->
  Resolver ->
  ModuleName ->
  Module ->
  Sem r ModuleInfo
loadParsedDiscoModule quiet resolver =
  loadParsedDiscoModule' quiet REPL resolver []

-- | Recursively load a Disco module while keeping track of an extra
--   Map from module names to 'ModuleInfo' records, to avoid loading
--   any imported module more than once. Resolve the module, load and
--   parse it, then call 'loadParsedDiscoModule''.
loadDiscoModule' ::
  Members '[State TopInfo, Output Message, Random, State Mem, Error DiscoError, Embed IO] r =>
  Bool ->
  Resolver ->
  [ModuleName] ->
  FilePath ->
  Sem r ModuleInfo
loadDiscoModule' quiet resolver inProcess modPath = do
  (resolvedPath, prov) <-
    resolveModule resolver modPath
      >>= maybe (throw $ ModuleNotFound modPath) return
  let name = Named prov modPath
  when (name `elem` inProcess) (throw $ CyclicImport (name : inProcess))
  modMap <- use @TopInfo topModMap
  case M.lookup name modMap of
    Just mi -> return mi
    Nothing -> do
      unless quiet $ info $ "Loading" <+> text (modPath -<.> "disco") Pretty.<> "..."
      cm <- parseDiscoModule resolvedPath
      loadParsedDiscoModule' quiet Standalone resolver (name : inProcess) name cm

-- | A list of standard library module names, which should always be
--   loaded implicitly.
stdLib :: [String]
stdLib = ["list", "container"]

-- | Recursively load an already-parsed Disco module while keeping
--   track of an extra Map from module names to 'ModuleInfo' records,
--   to avoid loading any imported module more than once.  Typecheck
--   it in the context of the top-level type context iff the
--   'LoadingMode' parameter is 'REPL'.  Recursively load all its
--   imports, then typecheck it.
loadParsedDiscoModule' ::
  Members '[State TopInfo, Output Message, Random, State Mem, Error DiscoError, Embed IO] r =>
  Bool ->
  LoadingMode ->
  Resolver ->
  [ModuleName] ->
  ModuleName ->
  Module ->
  Sem r ModuleInfo
loadParsedDiscoModule' quiet mode resolver inProcess name cm@(Module _ mns _ _ _) = do
  -- Recursively load any modules imported by this one, plus standard
  -- library modules (unless NoStdLib is enabled), and build a map with the results.
  mis <- mapM (loadDiscoModule' quiet (withStdlib resolver) inProcess) mns
  stdmis <- case NoStdLib `S.member` modExts cm of
    True -> return []
    False -> mapM (loadDiscoModule' True FromStdlib inProcess) stdLib
  let modImps = M.fromList (map (view miName &&& id) (mis ++ stdmis))

  -- Get context and type definitions from the REPL, in case we are in REPL mode.
  topImports <- use (replModInfo . miImports)
  topTyCtx <- use (replModInfo . miTys)
  topTyDefns <- use (replModInfo . miTydefs)

  -- Choose the contexts to use based on mode: if we are loading a
  -- standalone module, we should start it in an empty context.  If we
  -- are loading something entered at the REPL, we need to include any
  -- existing top-level REPL context.
  let importMap = case mode of Standalone -> modImps; REPL -> topImports <> modImps
      tyctx = case mode of Standalone -> emptyCtx; REPL -> topTyCtx
      tydefns = case mode of Standalone -> M.empty; REPL -> topTyDefns

  -- Typecheck (and resolve names in) the module.
  m <- runTCM tyctx tydefns $ checkModule name importMap cm

  -- Check for partial functions
  let allTyDefs = M.unionWith const tydefns (m ^. miTydefs)
  runFresh $ mapM_ (checkExhaustive allTyDefs) (Ctx.elems $ m ^. miTermdefs)

  -- Evaluate all the module definitions and add them to the topEnv.
  mapError EvalErr $ loadDefsFrom m

  -- Record the ModuleInfo record in the top-level map.
  modify (topModMap %~ M.insert name m)
  return m

-- | Try loading the contents of a file from the filesystem, emitting
--   an error if it's not found.
loadFile :: Members '[Output Message, Embed IO] r => FilePath -> Sem r (Maybe String)
loadFile file = do
  res <- liftIO $ handle @SomeException (return . Left) (Right <$> readFile file)
  case res of
    Left _ -> info ("File not found:" <+> text file) >> return Nothing
    Right s -> return (Just s)

-- | Add things from the given module to the set of currently loaded
--   things.
addToREPLModule ::
  Members '[Error DiscoError, State TopInfo, Random, State Mem, Output Message] r =>
  ModuleInfo ->
  Sem r ()
addToREPLModule mi = modify @TopInfo (replModInfo <>~ mi)

-- | Set the given 'ModuleInfo' record as the currently loaded
--   module. This also includes updating the top-level state with new
--   term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setREPLModule ::
  Members '[State TopInfo, Random, Error EvalError, State Mem, Output Message] r =>
  ModuleInfo ->
  Sem r ()
setREPLModule mi = do
  modify @TopInfo $ replModInfo .~ mi

-- | Populate various pieces of the top-level info record (docs, type
--   context, type and term definitions) from the 'ModuleInfo' record
--   corresponding to the currently loaded module, and load all the
--   definitions into the current top-level environment.
loadDefsFrom ::
  Members '[State TopInfo, Random, Error EvalError, State Mem] r =>
  ModuleInfo ->
  Sem r ()
loadDefsFrom mi = do
  -- Note that the compiled definitions we get back from compileDefns
  -- are topologically sorted by mutually recursive group. Each
  -- definition needs to be evaluated in an environment containing the
  -- previous ones.

  mapM_ (uncurry loadDef) (compileDefns (mi ^. miTermdefs))

loadDef ::
  Members '[State TopInfo, Random, Error EvalError, State Mem] r =>
  QName Core ->
  Core ->
  Sem r ()
loadDef x body = do
  v <- inputToState @TopInfo . inputTopEnv $ eval body
  modify @TopInfo $ topEnv %~ Ctx.insert x v

checkExhaustive :: Members '[Fresh, Output Message, Embed IO] r => TyDefCtx -> Defn -> Sem r ()
checkExhaustive tyDefCtx (Defn name argsType _ boundClauses) = do
  clauses <- NonEmpty.map fst <$> mapM unbind boundClauses
  runReader @TyDefCtx tyDefCtx $ checkClauses name argsType clauses
