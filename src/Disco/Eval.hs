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

         -- * Running things

       , runDisco
       , runTCM, runTCMWith
       , withTopEnv, inputTopEnv
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

import           Control.Exception        (SomeException, handle)
import           Control.Lens             (makeLenses, view, (%~), (.~))
import           Control.Monad            (void, when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Bifunctor
import           Data.Coerce
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           System.FilePath          ((-<.>))

import qualified System.Console.Haskeline as H

import           Disco.Effects.Error
import           Disco.Effects.Input
import           Disco.Effects.LFresh
import           Disco.Effects.Output
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
import           Disco.Interpret.CESK
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
  . mapError EvalErr         -- Embed runtime errors into top-level error type
  . failToError Panic        -- Turn pattern-match failures into a Panic error
  . runReader emptyCtx       -- Keep track of current Env

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
  => Resolver -> ModName -> Sem r ModuleInfo
loadDiscoModule resolver m =
  evalState M.empty $ loadDiscoModule' resolver S.empty m

-- | Like 'loadDiscoModule', but start with an already parsed 'Module'
--   instead of loading a module from disk by name.  Also, check it in
--   a context that includes the current top-level context (unlike a
--   module loaded from disk).  Used for e.g. blocks/modules entered
--   at the REPL prompt.
loadParsedDiscoModule
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO] r
  => Resolver -> ModName -> Module -> Sem r ModuleInfo
loadParsedDiscoModule resolver modName m =
  evalState M.empty $ loadParsedDiscoModule' REPL resolver S.empty modName m

-- | Recursively load a Disco module while keeping track of an extra
--   Map from module names to 'ModuleInfo' records, to avoid loading
--   any imported module more than once. Resolve the module, load and
--   parse it, then call 'loadParsedDiscoModule''.
loadDiscoModule'
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO, State (M.Map ModName ModuleInfo)] r
  => Resolver -> S.Set ModName -> ModName
  -> Sem r ModuleInfo
loadDiscoModule' resolver inProcess modName  = do
  when (S.member modName inProcess) (throw $ CyclicImport modName)
  modMap <- get
  case M.lookup modName modMap of
    Just mi -> return mi
    Nothing -> do
      file <- resolveModule resolver modName
              >>= maybe (throw $ ModuleNotFound modName) return
      outputLn $ "Loading " ++ (modName -<.> "disco") ++ "..."
      cm <- parseDiscoModule file
      loadParsedDiscoModule' Standalone resolver (S.insert modName inProcess) modName cm

-- | Recursively load an already-parsed Disco module while keeping
--   track of an extra Map from module names to 'ModuleInfo' records,
--   to avoid loading any imported module more than once.  Typecheck
--   it in the context of the top-level type context iff the
--   'LoadingMode' parameter is 'REPL'.  Recursively load all its
--   imports, then typecheck it.
loadParsedDiscoModule'
  :: Members '[Input TopInfo, Output String, Error DiscoError, Embed IO, State (M.Map ModName ModuleInfo)] r
  => LoadingMode -> Resolver -> S.Set ModName -> ModName -> Module -> Sem r ModuleInfo
loadParsedDiscoModule' mode resolver inProcess modName cm@(Module _ mns _ _ _) = do
  -- mis only contains the module info from direct imports.
  mis <- mapM (loadDiscoModule' (withStdlib resolver) inProcess) mns
  imports@(ModuleInfo _ _ tyctx tydefns _ _ _) <- mapError TypeCheckErr $ combineModuleInfo Standalone mis
  topTyCtx   <- inputs (view topCtx)
  topTyDefns <- inputs (view topTyDefs)
  let tyctx'   = case mode of { Standalone -> tyctx   ; REPL -> joinCtx topTyCtx tyctx }
  let tydefns' = case mode of { Standalone -> tydefns ; REPL -> M.union topTyDefns tydefns }
  m  <- runTCMWith tyctx' tydefns' $ checkModule cm
  m' <- mapError TypeCheckErr $ combineModuleInfo mode [imports, m]
  modify (M.insert modName m')
  return m'

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
  :: Members '[Error DiscoError, State TopInfo, Reader Env, Error EvalError] r
  => LoadingMode -> ModuleInfo -> Sem r ()
addModule mode mi = do
  curMI <- gets @TopInfo (view topModInfo)
  mi' <- mapError TypeCheckErr $ combineModuleInfo mode [curMI, mi]
  setLoadedModule mi'

-- | Set the given 'ModuleInfo' record as the currently loaded
--   module. This also includes updating the top-level state with new
--   term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule
  :: Members '[State TopInfo, Reader Env, Error EvalError] r
  => ModuleInfo -> Sem r ()
setLoadedModule mi = do
  modify @TopInfo $ topModInfo .~ mi
  populateCurrentModuleInfo

-- | Populate various pieces of the top-level info record (docs, type
--   context, type and term definitions) from the 'ModuleInfo' record
--   corresponding to the currently loaded module, and load all the
--   definitions into the current top-level environment.
populateCurrentModuleInfo
  :: Members '[State TopInfo, Reader Env, Error EvalError] r
  => Sem r ()
populateCurrentModuleInfo = do
  ModuleInfo docs _ tys tyds tmds _ _ <- gets @TopInfo (view topModInfo)
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
  :: Members '[Reader Env, State TopInfo, Error EvalError] r
  => Ctx Core Core -> Sem r ()
loadDefs defs = do
  -- XXX need to allow these to be recursive!
  -- Do a big "fix" at the beginning?  I forget how I figured out that
  -- could work...
  newEnv <- inputToState . inputTopEnv $ mapM eval defs
  modify @TopInfo $ topEnv %~ joinCtx newEnv
