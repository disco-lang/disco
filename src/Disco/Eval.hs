{-# OPTIONS_GHC -fno-warn-orphans     #-}
  -- For MonadFail instance; see below.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
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

         -- * Values

         Value(.., VFun, VDelay)
       , SimpleValue(..)

         -- * Props & testing
       , ValProp(..), TestResult(..), TestReason_(..), TestReason
       , SearchType(..), SearchMotive(.., SMExists, SMForall)
       , TestVars(..), TestEnv(..), emptyTestEnv, getTestEnv, extendPropEnv, extendResultEnv

         -- * Environments

       , Env, extendEnv, extendsEnv, getEnv, withEnv, withTopEnv
       , garbageCollect

         -- * Memory cells

       , Cell(..), mkCell, showMemory

         -- * Errors

       , IErr(..)

         -- * Disco monad state

       , Loc, DiscoState(..), initDiscoState

         -- ** Lenses

       , topModInfo, topCtx, topDefns, topTyDefns, topDocs
       , lastFile, enabledExts

         -- * Disco monad

       , Disco

         -- ** Utilities
       , io, iputStrLn, iputStr, iprint
       , emitMessage, info, warning, err, panic, debug
       , runDisco, catchAsMessage, catchAndPrintErrors

         -- ** Memory/environment utilities
       , allocate, delay, delay', mkValue, mkSimple

         -- ** Top level phases
       , adaptError
       , parseDiscoModule
       , typecheckDisco
       , loadDiscoModule

       )
       where

import           Text.Printf

import           Capability.Error
import           Capability.Reader
import           Capability.Sink                  (HasSink)
import           Capability.Source
import           Capability.State
import           Capability.Writer
import           Control.Lens                     (makeLenses)
import           Control.Monad                    (forM, forM_, when)
import qualified Control.Monad.Catch              as CMC
import qualified Control.Monad.Except             as CME
import qualified Control.Monad.Fail               as Fail
import           Control.Monad.IO.Class
import           Control.Monad.State              (StateT (..))
import qualified Control.Monad.State              as CMS
import           Data.IntMap.Lazy                 (IntMap)
import qualified Data.IntMap.Lazy                 as IntMap
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IntSet
import qualified Data.Map                         as M
import qualified Data.Sequence                    as Seq
import qualified Data.Set                         as S
import           Data.Void
import           GHC.Generics                     (Generic)
import           System.FilePath                  ((-<.>))
import           Text.Megaparsec                  hiding (runParser)

import           Unbound.Generics.LocallyNameless

import           Algebra.Graph                    (Graph)

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Capability
import           Disco.Context
import           Disco.Extensions
import           Disco.Messages
import           Disco.Module
import           Disco.Parser
import           Disco.Typecheck                  (checkModule)
import           Disco.Typecheck.Monad
import           Disco.Types

------------------------------------------------------------
-- Values
------------------------------------------------------------

-- | The type of values produced by the interpreter.
data Value where
  -- | A numeric value, which also carries a flag saying how
  --   fractional values should be diplayed.
  VNum   :: RationalDisplay -> Rational -> Value

  -- | A constructor with arguments.  The Int indicates which
  --   constructor it is.  For example, False is represented by
  --   @VCons 0 []@, and True by @VCons 1 []@.  A pair is
  --   represented by @VCons 0 [v1, v2]@, and @inr v@ by @VCons 1
  --   [v]@.
  VCons  :: Int -> [Value] -> Value

  -- | A built-in function constant.
  VConst :: Op -> Value

  -- | A closure, i.e. a function body together with its
  --   environment.
  VClos  :: Bind [Name Core] Core -> Env -> Value

  -- | A partial application, i.e. an application of a thing to some
  --   arguments which is still waiting for more.  Invariant: the
  --   thing being applied is in WHNF.
  VPAp   :: Value -> [Value] -> Value

  -- | A thunk, i.e. an unevaluated core expression together with
  --   its environment.
  VThunk :: Core -> Env -> Value

  -- | An indirection, i.e. a pointer to an entry in the value table.
  --   This is how we get graph reduction.  When we create a thunk, we
  --   put it in a new entry in the value table, and return a VIndir.
  --   The VIndir can get copied but all the copies refer to the same
  --   thunk, which will only be evaluated once, the first time the
  --   value is demanded.
  VIndir :: Loc -> Value

  -- | A literal function value.  @VFun@ is only used when
  --   enumerating function values in order to decide comparisons at
  --   higher-order function types.  For example, in order to
  --   compare two values of type @(Bool -> Bool) -> Bool@ for
  --   equality, we have to enumerate all functions of type @Bool ->
  --   Bool@ as @VFun@ values.
  --
  --   We assume that all @VFun@ values are /strict/, that is, their
  --   arguments should be fully evaluated to RNF before being
  --   passed to the function.
  VFun_   :: ValFun -> Value

  -- | A proposition.
  VProp   :: ValProp -> Value

  -- | A @Disco Value@ computation which can be run later, along with
  --   the environment in which it should run, and a set of referenced
  --   memory locations that should not be garbage collected.
  VDelay_  :: ValDelay -> IntSet -> Env -> Value

  -- | A literal bag, containing a finite list of (perhaps only
  --   partially evaluated) values, each paired with a count.  This is
  --   also used to represent sets (with the invariant that all counts
  --   are equal to 1).
  VBag :: [(Value, Integer)] -> Value

  -- | A Graph in the algebraic repesentation. The stored value is an indirection to the graph's adjacency map representation.
  VGraph :: Graph SimpleValue -> Value -> Value

  -- | A map from keys to values. Differs from functions because we can
  --   actually construct the set of entries, while functions only have this
  --   property when the key type is finite.
  VMap :: M.Map SimpleValue Value -> Value

  -- | A disco type can be a value.  For now, there are only a very
  --   limited number of places this could ever show up (in
  --   particular, as an argument to @enumerate@ or @count@).
  VType :: Type -> Value
  deriving Show

-- | Values which can be used as keys in a map, i.e. those for which a
--   Haskell Ord instance can be easily created.  These should always
--   be of a type for which the QSimple qualifier can be constructed.
--   At the moment these are always fully evaluated (containing no
--   indirections) and thus don't need memory management.  At some
--   point in the future constructors for simple graphs and simple
--   maps could be created, if the value type is also QSimple.  The
--   only reason for actually doing this would be constructing graphs
--   of graphs or maps of maps, or the like.
data SimpleValue where
  SNum   :: RationalDisplay -> Rational -> SimpleValue
  SCons  :: Int -> [SimpleValue] -> SimpleValue
  SBag   :: [(SimpleValue, Integer)] -> SimpleValue
  SType  :: Type -> SimpleValue
  deriving (Show, Eq, Ord)

-- | A @ValFun@ is just a Haskell function @Value -> Value@.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValFun = ValFun (Value -> Value)

instance Show ValFun where
  show _ = "<fun>"

pattern VFun :: (Value -> Value) -> Value
pattern VFun f = VFun_ (ValFun f)

-- | A @ValDelay@ is just a @Disco Value@ computation.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValDelay = ValDelay (Disco Value)

instance Show ValDelay where
  show _ = "<delay>"

pattern VDelay :: Disco Value -> IntSet -> Env -> Value
pattern VDelay m ls e = VDelay_ (ValDelay m) ls e

------------------------------------------------------------
-- Propositions
------------------------------------------------------------

data SearchType
  = Exhaustive
    -- ^ All possibilities were checked.
  | Randomized Integer Integer
    -- ^ A number of small cases were checked exhaustively and
    --   then a number of additional cases were checked at random.
  deriving Show

-- | The answer (success or failure) we're searching for, and
--   the result (success or failure) we return when we find it.
--   The motive @(False, False)@ corresponds to a "forall" quantifier
--   (look for a counterexample, fail if you find it) and the motive
--   @(True, True)@ corresponds to "exists". The other values
--   arise from negations.
newtype SearchMotive = SearchMotive (Bool, Bool)
  deriving Show

pattern SMForall :: SearchMotive
pattern SMForall = SearchMotive (False, False)

pattern SMExists :: SearchMotive
pattern SMExists = SearchMotive (True, True)

-- | A collection of variables that might need to be reported for
--   a test, along with their types and user-legible names.
newtype TestVars = TestVars [(String, Type, Name Core)]
  deriving (Show, Semigroup, Monoid)

-- | A variable assignment found during a test.
newtype TestEnv = TestEnv [(String, Type, Value)]
  deriving (Show, Semigroup, Monoid)

emptyTestEnv :: TestEnv
emptyTestEnv = TestEnv []

getTestEnv :: Has '[Rd "env", Th "err"] m => TestVars -> m TestEnv
getTestEnv (TestVars tvs) = fmap TestEnv . forM tvs $ \(s, ty, name) -> do
  value <- M.lookup name <$> getEnv
  case value of
    Just v  -> return (s, ty, v)
    Nothing -> throw @"err" (UnboundError name)

-- | The possible outcomes of a property test, parametrized over
--   the type of values. A @TestReason@ explains why a proposition
--   succeeded or failed.
data TestReason_ a
  = TestBool
    -- ^ The prop evaluated to a boolean.
  | TestEqual Type a a
    -- ^ The test was an equality test. Records the values being
    --   compared and also their type (which is needed for printing).
  | TestNotFound SearchType
    -- ^ The search didn't find any examples/counterexamples.
  | TestFound TestResult
    -- ^ The search found an example/counterexample.
  | TestRuntimeError IErr
    -- ^ The prop failed at runtime. This is always a failure, no
    --   matter which quantifiers or negations it's under.
  deriving (Show, Functor, Foldable, Traversable)

type TestReason = TestReason_ Value

-- | The possible outcomes of a proposition.
data TestResult = TestResult Bool TestReason TestEnv
  deriving Show

-- | A @ValProp@ is the normal form of a Disco value of type @Prop@.
data ValProp
  = VPDone TestResult
    -- ^ A prop that has already either succeeded or failed.
  | VPSearch SearchMotive [Type] Value TestEnv
    -- ^ A pending search.
  deriving Show

extendPropEnv :: TestEnv -> ValProp -> ValProp
extendPropEnv g (VPDone (TestResult b r e)) = VPDone (TestResult b r (g <> e))
extendPropEnv g (VPSearch sm tys v e)       = VPSearch sm tys v (g <> e)

extendResultEnv :: TestEnv -> TestResult -> TestResult
extendResultEnv g (TestResult b r e) = TestResult b r (g <> e)

------------------------------------------------------------
-- Environments
------------------------------------------------------------

-- | An environment is a mapping from names to values.
type Env  = Ctx Core Value

-- | Locally extend the environment with a new name -> value mapping,
--   (shadowing any existing binding for the given name).
extendEnv :: Has '[Rd "env", LFresh] m => Name Core -> Value -> m a -> m a
extendEnv x v = avoid [AnyName x] . extend @"env" x v

-- | Locally extend the environment with another environment.
--   Bindings in the new environment shadow bindings in the old.
extendsEnv :: Has '[Rd "env", LFresh] m => Env -> m a -> m a
extendsEnv e' = avoid (map AnyName (names e')) . extends @"env" e'

-- | Get the current environment.
getEnv :: Has '[Rd "env"] m => m Env
getEnv = ask @"env"

-- | Run a @Disco@ computation with a /replaced/ (not extended)
--   environment.  This is used for evaluating things such as closures
--   and thunks that come with their own environment.
withEnv :: Has '[Rd "env"] m => Env -> m a -> m a
withEnv = local @"env" . const

------------------------------------------------------------
-- Memory cells
------------------------------------------------------------

data Cell = Cell { cellVal :: Value, cellIsWHNF :: Bool }
  deriving (Show)

mkCell :: Value -> Cell
mkCell v = Cell v False

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | Errors that can be generated during interpreting.
data IErr where

  -- | Module not found.
  ModuleNotFound :: ModName -> IErr

  -- | Cyclic import encountered.
  CyclicImport :: ModName -> IErr

  -- | Error encountered during typechecking.
  TypeCheckErr :: TCError -> IErr

  -- | Error encountered during parsing.
  ParseErr :: ParseErrorBundle String Data.Void.Void -> IErr

  -- | An unbound name.
  UnboundError  :: Name Core -> IErr

  -- | Division by zero.
  DivByZero     ::              IErr

  -- | Underflow, e.g. (2 - 3 : Nat)
  Underflow     ::              IErr

  -- | Overflow, e.g. (2^66)!
  Overflow      ::              IErr

  -- | Non-exhaustive case analysis.
  NonExhaustive ::              IErr

  -- | Trying to count an infinite type.
  InfiniteTy    :: Type      -> IErr

  -- | User-generated crash.
  Crash         :: String    -> IErr

  deriving Show

------------------------------------------------------------
-- Disco monad state
------------------------------------------------------------

-- | A location in memory is represented by an @Int@.
type Loc = Int

type Memory = IntMap Cell

-- | The various pieces of state tracked by the 'Disco' monad.
data DiscoState = DiscoState
  { _topModInfo  :: ModuleInfo
    -- ^ Info about the top-level currently loaded module.  Due to
    --   import statements this may actually be a combination of info
    --   about multiple physical modules.

  , _topCtx      :: Ctx Term PolyType
    -- ^ Top-level type environment.

  , _topDefns    :: Ctx ATerm Defn
    -- ^ Environment of top-level surface syntax definitions.  Set by
    --   'loadDefs' and by 'let' command at the REPL.

  , _topTyDefns  :: TyDefCtx
    -- ^ Environment of top-level type definitions.

  , _topEnv      :: Env
    -- ^ Top-level environment mapping names to values (which all
    --   start as indirections to thunks).  Set by 'loadDefs'.
    --   Use it when evaluating with 'withTopEnv'.

  , _localEnv    :: Env
    -- ^ Local environment used during evaluation of expressions.

  , _topDocs     :: Ctx Term Docs
    -- ^ Top-level documentation.

  , _memory      :: Memory
    -- ^ A memory is a mapping from "locations" (uniquely generated
    --   identifiers) to values, along with a flag saying whether the
    --   value has been evaluated yet.  It also keeps track of the
    --   next unused location.  We keep track of a memory during
    --   evaluation, and can create new memory locations to store
    --   things that should only be evaluated once.

  , _nextLoc     :: Loc
    -- ^ The next available (unused) memory location.

  , _messageLog  :: MessageLog IErr
    -- ^ A stream of messages generated by the system.

  , _lastFile    :: Maybe FilePath
    -- ^ The most recent file which was :loaded by the user.

  , _enabledExts :: ExtSet
    -- ^ The set of language extensions currently enabled in the REPL.
    --   Note this affects only expressions entered at the REPL
    --   prompt, not modules loaded into the REPL; each module
    --   specifies its own extensions.
  }
  deriving (Generic)

-- | The initial state for the @Disco@ monad.
initDiscoState :: DiscoState
initDiscoState = DiscoState
  { _topModInfo    = emptyModuleInfo
  , _topCtx        = emptyCtx
  , _topDefns      = emptyCtx
  , _topTyDefns    = M.empty
  , _topDocs       = emptyCtx
  , _topEnv        = emptyCtx
  , _localEnv      = emptyCtx
  , _memory        = IntMap.empty
  , _nextLoc       = 0
  , _messageLog    = emptyMessageLog
  , _lastFile      = Nothing
  , _enabledExts   = defaultExts
  }

------------------------------------------------------------
-- Disco monad
------------------------------------------------------------

type DiscoM = StateT DiscoState (CME.ExceptT IErr (LFreshMT IO))

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
  deriving (Functor, Applicative, Monad, LFresh, MonadIO, CMS.MonadState DiscoState, MonadFail, CMC.MonadThrow, CMC.MonadCatch, CMC.MonadMask)
  deriving (HasReader "env" Env, HasSource "env" Env) via
    (ReadStatePure
    (Rename "_localEnv"
    (Field "_localEnv" ()
    (MonadState DiscoM))))
  deriving (HasThrow "err" IErr, HasCatch "err" IErr) via
    (MonadError DiscoM)
  deriving (HasWriter "msg" (MessageLog IErr), HasSink "msg" (MessageLog IErr)) via
    (WriterLog
    (Rename "_messageLog"
    (Field "_messageLog" ()
    (MonadState DiscoM))))
  deriving (HasState "mem" Memory, HasSource "mem" Memory, HasSink "mem" Memory) via
    (Rename "_memory"
    (Field "_memory" ()
    (MonadState DiscoM)))
  deriving (HasSource "nextloc" Loc) via
    (Counter
    (Rename "_nextLoc"
    (Field "_nextLoc" ()
    (MonadState DiscoM))))
  deriving (HasState "topenv" Env, HasSource "topenv" Env, HasSink "topenv" Env) via
    (Rename "_topEnv"
    (Field "_topEnv" ()
    (MonadState DiscoM)))

newtype Counter m a = Counter (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, HasState tag Loc m) => HasSource (tag :: k) Loc (Counter m) where
  await_ tag = Counter $ state_ tag $ \i -> (i,i+1)

type instance TypeOf _ "env"     = Env
type instance TypeOf _ "err"     = IErr
type instance TypeOf _ "msg"     = MessageLog IErr
type instance TypeOf _ "mem"     = Memory
type instance TypeOf _ "nextloc" = Loc
type instance TypeOf _ "topenv"  = Env

------------------------------------------------------------
-- Some needed instances.

-- This was introduced in unbound-generics-0.4.1.  Once we start
-- building with that version, this orphan instance can be removed
-- (and we can also remove the -fno-warn-orphans flag).
instance MonadFail m => MonadFail (LFreshMT m) where
  fail = LFreshMT . Fail.fail

------------------------------------------------------------
-- Lenses for state
------------------------------------------------------------

makeLenses ''DiscoState

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io = liftIO

iputStrLn :: MonadIO m => String -> m ()
iputStrLn = io . putStrLn

iputStr :: MonadIO m => String -> m ()
iputStr = io . putStr

iprint :: (MonadIO m, Show a) => a -> m ()
iprint = io . print

emitMessage :: Has '[Wr "msg"] m => MessageLevel -> MessageBody IErr -> m ()
emitMessage lev body = tell @"msg" $ Seq.singleton (Message lev body)

info, warning, err, panic, debug :: Has '[Wr "msg"] m => MessageBody IErr -> m ()
info    = emitMessage Info
warning = emitMessage Warning
err     = emitMessage Error
panic   = emitMessage Panic
debug   = emitMessage Debug

-- | Run a computation in the @Disco@ monad, starting in the empty
--   environment.
runDisco :: Disco a -> IO (Either IErr a)
runDisco
  = runLFreshMT
  . CME.runExceptT
  . flip CMS.evalStateT initDiscoState
  . unDisco

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
delay :: Disco Value -> Disco Value
delay = delay' []

-- | Like 'delay', but also specify a set of values which will be
--   needed during the delayed computation, to prevent any memory
--   referenced by the values from being garbage collected.
delay' :: [Value] -> Disco Value -> Disco Value
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

-- | Run a computation with the top-level environment used as the
--   current local environment.  For example, this is used every time
--   we start evaluating an expression entered at the command line.
withTopEnv :: Has '[Rd "env", St "topenv"] m => m a -> m a
withTopEnv m = do
  env <- get @"topenv"
  withEnv env m

-- | Deallocate any memory cells which are no longer referred to by
--   any top-level binding.
garbageCollect :: Has '[St "topenv", St "mem", MonadIO] m => m ()
garbageCollect = do
  env  <- get @"topenv"
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

showMemory :: Disco ()
showMemory = get @"mem" >>= (mapM_ showCell . IntMap.assocs)
  where
    showCell :: (Int, Cell) -> Disco ()
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
