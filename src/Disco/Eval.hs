{-# OPTIONS_GHC -fno-warn-orphans     #-}
  -- For MonadFail instance; see below.

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Disco values and evaluation monad. This is the main monad used in
-- the REPL and for evaluation and pretty-printing.
--
-----------------------------------------------------------------------------

module Disco.Eval
       (

         -- * Memory

         Cell(..), mkCell, Loc, Memory, showMemory, garbageCollect
       , withTopEnv

         -- * Errors

       , IErr(..)

         -- * Disco monad state

       , DiscoState(..), initDiscoState

         -- * Lenses for top-level info record

       , topModInfo, topCtx, topDefs, topTyDefs, topEnv, topDocs

         -- * Disco monad

       , Disco, MonadDisco, runDisco

         -- ** Messages
       , emitMessage, info, warning, err, panic, debug
       , catchAsMessage, catchAndPrintErrors

         -- ** Memory/environment utilities
       , allocate, delay, delay', mkValue, mkSimple

         -- ** Top level phases
       , adaptError
       , parseDiscoModule
       , typecheckDisco
       , loadDiscoModule

       )
       where

import           Capability.Error
import           Capability.Reader
import           Capability.Sink                  (HasSink)
import           Capability.Source
import           Capability.State
import           Capability.Writer
import           Control.Monad                    (forM_, when)
import qualified Control.Monad.Catch              as CMC
import qualified Control.Monad.Fail               as Fail
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (ReaderT (..))
import qualified Control.Monad.Reader             as CMR
import           Data.IORef                       (IORef, newIORef)
import           Data.IntMap.Lazy                 (IntMap)
import qualified Data.IntMap.Lazy                 as IntMap
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IntSet
import qualified Data.Map                         as M
import qualified Data.Sequence                    as Seq
import qualified Data.Set                         as S
import           GHC.Generics                     (Generic)
import           System.FilePath                  ((-<.>))
import           Text.Printf

import           Control.Lens                     (makeLenses, view)
import           Text.Megaparsec                  hiding (runParser)
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Capability
import           Disco.Context
import           Disco.Error
import           Disco.Extensions
import           Disco.Messages
import           Disco.Module
import           Disco.Parser
import           Disco.Typecheck                  (checkModule)
import           Disco.Typecheck.Monad
import           Disco.Types
import           Disco.Util
import           Disco.Value

------------------------------------------------------------
-- Memory cells
------------------------------------------------------------

-- | A memory cell holds a value, along with a flag recording whether
--   the value has been reduced to WHNF.
data Cell = Cell { cellVal :: Value, cellIsWHNF :: Bool }
  deriving (Show)

-- | Create a memory cell from a value, with the WHNF flag initially
--   set to false.
mkCell :: Value -> Cell
mkCell v = Cell v False

-- | A location in memory is represented by an @Int@.
type Loc = Int

-- | Memory is a collection of cells indexed by location.
type Memory = IntMap Cell

------------------------------------------------------------
-- Disco monad state
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
  }
  deriving (Generic)

-- | The various pieces of state tracked by the 'Disco' monad.
data DiscoState = DiscoState
  { _topInfo     :: IORef TopInfo
    -- ^ Information about the current top-level environment
    --   (definitions, types, documentation, etc.).

  , _localEnv    :: Env
    -- ^ Local environment used during evaluation of expressions.

  , _memory      :: IORef Memory
    -- ^ A memory is a mapping from "locations" (uniquely generated
    --   identifiers) to values, along with a flag saying whether the
    --   value has been evaluated yet.  It also keeps track of the
    --   next unused location.  We keep track of a memory during
    --   evaluation, and can create new memory locations to store
    --   things that should only be evaluated once.

  , _nextLoc     :: IORef Loc
    -- ^ The next available (unused) memory location.

  , _messageLog  :: IORef (MessageLog IErr)
    -- ^ A stream of messages generated by the system.

  , _lastFile    :: IORef (Maybe FilePath)
    -- ^ The most recent file which was :loaded by the user.

  , _enabledExts :: IORef ExtSet
    -- ^ The set of language extensions currently enabled in the REPL.
    --   Note this affects only expressions entered at the REPL
    --   prompt, not modules loaded into the REPL; each module
    --   specifies its own extensions.
  }
  deriving (Generic)

initTopInfo :: TopInfo
initTopInfo = TopInfo
  { _topModInfo = emptyModuleInfo
  , _topCtx     = emptyCtx
  , _topDefs    = emptyCtx
  , _topTyDefs  = M.empty
  , _topDocs    = emptyCtx
  , _topEnv     = emptyCtx
  }

-- | The initial state for the @Disco@ monad.
initDiscoState :: IO DiscoState
initDiscoState = do
  topInfoRef    <- newIORef initTopInfo
  memoryRef     <- newIORef IntMap.empty
  nextLocRef    <- newIORef 0
  messageLogRef <- newIORef emptyMessageLog
  lastFileRef   <- newIORef Nothing
  extsRef       <- newIORef defaultExts
  return $ DiscoState
    { _topInfo       = topInfoRef
    , _localEnv      = emptyCtx
    , _memory        = memoryRef
    , _nextLoc       = nextLocRef
    , _messageLog    = messageLogRef
    , _lastFile      = lastFileRef
    , _enabledExts   = extsRef
    }

------------------------------------------------------------
-- Disco monad
------------------------------------------------------------

type DiscoM = ReaderT DiscoState (LFreshMT IO)

-- | The main monad used by the Disco REPL, and by the interpreter and
--   pretty printer.
--
--   XXX redo this comment
--
--   * Keeps track of state such as current top-level definitions,
--     types, and documentation, and memory for allocation of thunks.
--   * Keeps track of a read-only environment for binding local
--     variables to their values.
--   * Can throw exceptions of type @e@
--   * Can log messages (errors, warnings, etc.)
--   * Can generate fresh names
--   * Can do I/O
newtype Disco a = Disco { unDisco :: DiscoM a }
  deriving newtype (Functor, Applicative, Monad, LFresh, MonadIO, CMR.MonadReader DiscoState, MonadFail, CMC.MonadThrow, CMC.MonadCatch, CMC.MonadMask)
  deriving (HasState "top" TopInfo, HasSource "top" TopInfo, HasSink "top" TopInfo) via
    (ReaderIORef
    (Rename "_topInfo"
    (Field "_topInfo" ()
    (MonadReader DiscoM))))
  deriving (HasReader "env" Env, HasSource "env" Env) via
    (Rename "_localEnv"
    (Field "_localEnv" ()
    (MonadReader DiscoM)))
  deriving (HasThrow "err" IErr, HasCatch "err" IErr) via
    (SafeExceptions IErr DiscoM)
  deriving (HasWriter "msg" (MessageLog IErr), HasSink "msg" (MessageLog IErr)) via
    (WriterLog
    (ReaderIORef
    (Rename "_messageLog"
    (Field "_messageLog" ()
    (MonadReader DiscoM)))))
  deriving (HasState "mem" Memory, HasSource "mem" Memory, HasSink "mem" Memory) via
    (ReaderIORef
    (Rename "_memory"
    (Field "_memory" ()
    (MonadReader DiscoM))))
  deriving (HasSource "nextloc" Loc) via
    (Counter
    (ReaderIORef
    (Rename "_nextLoc"
    (Field "_nextLoc" ()
    (MonadReader DiscoM)))))
  deriving (HasState "exts" ExtSet, HasSource "exts" ExtSet, HasSink "exts" ExtSet) via
    (ReaderIORef
    (Rename "_enabledExts"
    (Field "_enabledExts" ()
    (MonadReader DiscoM))))
  deriving (HasState "lastfile" (Maybe FilePath), HasSource "lastfile" (Maybe FilePath), HasSink "lastfile" (Maybe FilePath)) via
    (ReaderIORef
    (Rename "_lastFile"
    (Field "_lastFile" ()
    (MonadReader DiscoM))))

newtype Counter m a = Counter (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Monad m, HasState tag Loc m) => HasSource (tag :: k) Loc (Counter m) where
  await_ tag = Counter $ state_ tag $ \i -> (i,i+1)

type instance TypeOf _ "err"      = IErr
type instance TypeOf _ "msg"      = MessageLog IErr
type instance TypeOf _ "mem"      = Memory
type instance TypeOf _ "nextloc"  = Loc
type instance TypeOf _ "top"      = TopInfo
type instance TypeOf _ "exts"     = ExtSet
type instance TypeOf _ "lastfile" = Maybe FilePath

------------------------------------------------------------
-- Some needed instances.

-- This was introduced in unbound-generics-0.4.1.  Once we start
-- building with that version, this orphan instance can be removed
-- (and we can also remove the -fno-warn-orphans flag).
instance MonadFail m => MonadFail (LFreshMT m) where
  fail = LFreshMT . Fail.fail

------------------------------------------------------------
-- Running top-level Disco computations
------------------------------------------------------------

-- | Run a computation in the @Disco@ monad, starting in the empty
--   environment.
runDisco :: Disco a -> IO (Either IErr a)
runDisco d = do
  s <- initDiscoState
  flip CMC.catch (return . Left)
    . fmap Right
    . runLFreshMT
    . flip CMR.runReaderT s
    . unDisco
    $ d

------------------------------------------------------------
-- Lenses
------------------------------------------------------------

makeLenses ''TopInfo

------------------------------------------------------------
-- Messages
------------------------------------------------------------

emitMessage :: Has '[Wr "msg"] m => MessageLevel -> MessageBody IErr -> m ()
emitMessage lev body = tell @"msg" $ Seq.singleton (Message lev body)

info, warning, err, panic, debug :: Has '[Wr "msg"] m => MessageBody IErr -> m ()
info    = emitMessage Info
warning = emitMessage Warning
err     = emitMessage Error
panic   = emitMessage Panic
debug   = emitMessage Debug

-- | Run a @Disco@ computation; if it throws an exception, catch it
--   and turn it into a message.
catchAsMessage :: Has '[Ct "err", Wr "msg"] m => m () -> m ()
catchAsMessage m = catch @"err" m (err . Item)

-- XXX eventually we should get rid of this and replace with catchAsMessage
catchAndPrintErrors :: Has '[Ct "err", MonadIO] m => a -> m a -> m a
catchAndPrintErrors a m = catch @"err" m (\e -> handler e >> return a)
  where
    handler (ParseErr e)     = iputStrLn $ errorBundlePretty e
    handler (TypeCheckErr e) = iprint e
    handler e                = iprint e

------------------------------------------------------------
-- Memory/environment utilities
------------------------------------------------------------

-- | Run a computation with the top-level environment used as the
--   current local environment.  For example, this is used every time
--   we start evaluating an expression entered at the command line.
withTopEnv :: Has '[Rd "env", St "top"] m => m a -> m a
withTopEnv m = do
  env <- gets @"top" (view topEnv)
  withEnv env m

-- | Allocate a new memory cell for the given value, and return its
--   'Loc'.
allocate :: Has '[Sc "nextloc", St "mem"] m => Value -> m Loc
allocate v = do
  loc <- await @"nextloc"
  -- io $ putStrLn $ "allocating " ++ show v ++ " at location " ++ show loc
  modify @"mem" $ IntMap.insert loc (mkCell v)
  return loc

-- | Turn a value into a "simple" value which takes up a constant
--   amount of space: some are OK as they are; for others, we turn
--   them into an indirection and allocate a new memory cell for them.
mkSimple :: Has '[Sc "nextloc", St "mem"] m => Value -> m Value
mkSimple v@VNum{}       = return v
mkSimple v@(VCons _ []) = return v
mkSimple v@VConst{}     = return v
mkSimple v@VClos{}      = return v
mkSimple v@VType{}      = return v
mkSimple v@VIndir{}     = return v
mkSimple v              = VIndir <$> allocate v

-- | Delay a @Disco Value@ computation by packaging it into a
--   @VDelay@ constructor along with the current environment.
delay :: MonadDisco m => (forall m'. MonadDisco m' => m' Value) -> m Value
delay = delay' []

-- | Like 'delay', but also specify a set of values which will be
--   needed during the delayed computation, to prevent any memory
--   referenced by the values from being garbage collected.
delay' :: MonadDisco m => [Value] -> (forall m'. MonadDisco m' => m' Value) -> m Value
delay' vs imv = do
  ls <- getReachable vs
  VDelay imv ls <$> getEnv

-- | Turn a Core expression into a value.  Some kinds of expressions
--   can be turned into corresponding values directly; for others we
--   create a thunk by packaging up the @Core@ expression with the
--   current environment.  The thunk is stored in a new location in
--   memory, and the returned value consists of an indirection
--   referring to its location.
mkValue :: Has '[Rd "env", Sc "nextloc", St "mem"] m => Core -> m Value
mkValue (CConst op)  = return $ VConst op
mkValue (CCons i cs) = VCons i <$> mapM mkValue cs
mkValue (CNum d r)   = return $ VNum d r
mkValue (CType ty)   = return $ VType ty
mkValue c            = VIndir <$> (allocate . VThunk c =<< getEnv)

-- | Deallocate any memory cells which are no longer referred to by
--   any top-level binding.
garbageCollect :: Has '[St "top", St "mem", MonadIO] m => m ()
garbageCollect = do
  env  <- gets @"top" (view topEnv)
  keep <- getReachable env
  modify @"mem" $ \mem -> IntMap.withoutKeys mem (IntMap.keysSet mem `IntSet.difference` keep)

-- | Get the set of memory locations reachable from a set of values.
getReachable :: (Reachable v, Has '[St "mem", MonadIO] m) => v -> m IntSet
getReachable v =
  fmap snd $
    withLocalState @_ @"reachables" @IntSet @'[St "mem"] IntSet.empty $
      reachable v

class Reachable v where
  -- | @reachable v@ marks the memory locations reachable from the
  --   values stored in @v@.
  reachable :: Has '[St "mem", HasState "reachables" IntSet] m => v -> m ()

instance Reachable Value where
  reachable (VCons _ vs)    = reachable vs
  reachable (VClos _ e)     = reachable e
  reachable (VPAp v vs)     = reachable (v:vs)
  reachable (VThunk _ e)    = reachable e
  reachable (VIndir l)      = reachable l
  reachable (VDelay _ ls e) = (modify @"reachables" $ IntSet.union ls) >> reachable e
  reachable (VBag vs)       = reachable (map fst vs)
  reachable (VProp p)       = reachable p
  reachable (VGraph _ adj)  = reachable adj
    -- A graph can only contain SimpleValues, which by def contain no indirection.
    -- However its buffered adjacency map can.
  reachable (VMap m)        = reachable (M.elems m)
  reachable _               = return ()

instance Reachable Env where
  reachable = reachable . M.elems

instance Reachable v => Reachable [v] where
  reachable = mapM_ reachable

instance Reachable ValProp where
  reachable (VPDone (TestResult _ r vs)) = mapM_ reachable r >> reachable vs
  reachable (VPSearch _ _ v vs)          = reachable v >> reachable vs

instance Reachable TestEnv where
  reachable (TestEnv te) = forM_ te $ \(_, _, v) -> reachable v

instance Reachable Loc where
  reachable l = do
    reach <- get @"reachables"
    case IntSet.member l reach of
      True -> return ()
      False -> do
        modify @"reachables" $ IntSet.insert l
        mem <- get @"mem"
        case IntMap.lookup l mem of
          Nothing         -> return ()
          Just (Cell v _) -> reachable v

showMemory :: Has '[St "mem", MonadIO] m => m ()
showMemory = get @"mem" >>= (mapM_ showCell . IntMap.assocs)
  where
    showCell :: MonadIO m => (Int, Cell) -> m ()
    showCell (i, Cell v b) = liftIO $ printf "%3d%s %s\n" i (if b then "!" else " ") (show v)

------------------------------------------------------------
-- High-level disco phases
------------------------------------------------------------

-- | Utility function: given an 'Either', wrap a 'Left' in the given
--   function and throw it as a 'Disco' error, or return a 'Right'.
adaptError :: Has '[Th "err"] m => (e -> IErr) -> Either e a -> m a
adaptError f = either (throw @"err" . f) return

-- | Parse a module from a file, re-throwing a parse error if it
--   fails.
parseDiscoModule :: Has '[Th "err", MonadIO] m => FilePath -> m Module
parseDiscoModule file = do
  str <- io $ readFile file
  adaptError ParseErr $ runParser wholeModule file str

-- | Run a typechecking computation, re-throwing a wrapped error if it
--   fails.
typecheckDisco :: Has '[Th "err"] m => TyCtx -> TyDefCtx -> TCM a -> m a
typecheckDisco tyctx tydefs tcm =
  adaptError TypeCheckErr $ evalTCM (withTyDefns tydefs . extends @"tyctx" tyctx $ tcm)

-- | Recursively loads a given module by first recursively loading and
--   typechecking its imported modules, adding the obtained
--   'ModuleInfo' records to a map from module names to info records,
--   and then typechecking the parent module in an environment with
--   access to this map. This is really just a depth-first search.
--
--   If the given directory is Just, it will only load a module from
--   the specific given directory.  If it is Nothing, then it will look for
--   the module in the current directory or the standard library.
loadDiscoModule :: Has '[Th "err", MonadIO] m => Resolver -> ModName -> m ModuleInfo
loadDiscoModule resolver m =
  fmap fst $
    withLocalState @_ @"modmap" @(M.Map ModName ModuleInfo) @'[Th "err"] M.empty $
      loadDiscoModule' resolver S.empty m

-- | Recursively load a Disco module while keeping track of an extra
--   Map from module names to 'ModuleInfo' records, to avoid loading
--   any imported module more than once.
loadDiscoModule' ::
  Has '[Th "err", MonadIO, HasState "modmap" (M.Map ModName ModuleInfo)] m =>
  Resolver -> S.Set ModName -> ModName ->
  m ModuleInfo
loadDiscoModule' resolver inProcess modName  = do
  when (S.member modName inProcess) (throw @"err" $ CyclicImport modName)
  modMap <- get @"modmap"
  case M.lookup modName modMap of
    Just mi -> return mi
    Nothing -> do
      file <- resolveModule resolver modName
             >>= maybe (throw @"err" $ ModuleNotFound modName) return
      io . putStrLn $ "Loading " ++ (modName -<.> "disco") ++ "..."
      cm@(Module _ mns _ _) <- parseDiscoModule file

      -- mis only contains the module info from direct imports.
      mis <- mapM (loadDiscoModule' (withStdlib resolver) (S.insert modName inProcess)) mns
      imports@(ModuleInfo _ _ tyctx tydefns _) <- adaptError TypeCheckErr $ combineModuleInfo mis
      m  <- typecheckDisco tyctx tydefns (checkModule cm)
      m' <- adaptError TypeCheckErr $ combineModuleInfo [imports, m]
      modify @"modmap" (M.insert modName m')
      return m'
