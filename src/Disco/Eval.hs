{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE TemplateHaskell          #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Eval
-- Copyright   :  (c) 2017 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Disco values and evaluation monad. This is the main monad used in
-- the REPL and for evaluation and pretty-printing.
--
-----------------------------------------------------------------------------

module Disco.Eval
       (

         -- * Values

         Value(..), ValFun(..), ValDelay(..)

         -- * Environments

       , Env, extendEnv, extendsEnv, getEnv, withEnv

         -- * Errors

       , InterpError(..)

         -- * Disco monad state

       , Loc, DiscoState(..), initDiscoState

         -- ** Lenses

       , topCtx, topDefns, topDocs, memory, nextLoc

         -- * Disco monad

       , Disco

         -- ** Utilities
       , runDisco
       , allocate, delay, mkThunk

       )
       where

import           Control.Lens                       ((<+=), (%=), makeLenses)
import           Control.Monad.Trans.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict
import           Data.IntMap.Lazy                   (IntMap)
import qualified Data.IntMap.Lazy                   as IntMap

import           Unbound.Generics.LocallyNameless

import           System.Console.Haskeline.MonadException

import           Disco.Context
import           Disco.Types
import           Disco.AST.Core
import           Disco.AST.Surface

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

  -- | A closure, i.e. a function body together with its
  --   environment.
  VClos  :: Bind (Name Core) Core -> Env -> Value

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
  VFun   :: ValFun -> Value

  -- | A delayed value, containing a @Disco Value@ computation which can
  --   be run later.
  VDelay  :: ValDelay -> Value
  deriving Show

-- | A @ValFun@ is just a Haskell function @Value -> Value@.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValFun = ValFun (Value -> Value)

instance Show ValFun where
  show _ = "<fun>"

-- | A @ValDelay@ is just a @Disco Value@ computation.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValDelay = ValDelay (Disco Value)

instance Show ValDelay where
  show _ = "<delay>"

------------------------------------------------------------
-- Environments
------------------------------------------------------------

-- | An environment is a mapping from names to values.
type Env  = Ctx Core Value

-- | Locally extend the environment with a new name -> value mapping,
--   (shadowing any existing binding for the given name).
extendEnv :: Name Core -> Value -> Disco a -> Disco a
extendEnv x v = avoid [AnyName x] . extend x v

-- | Locally extend the environment with another environment.
--   Bindings in the new environment shadow bindings in the old.
extendsEnv :: Env -> Disco a -> Disco a
extendsEnv e' = avoid (map AnyName (names e')) . extends e'

-- | Get the current environment.
getEnv :: Disco Env
getEnv = ask

-- | Run a @Disco@ computation with a /replaced/ (not extended)
--   environment.  This is used for evaluating things such as closures
--   and thunks that come with their own environment.
withEnv :: Env -> Disco a -> Disco a
withEnv = local . const

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | Errors that can be generated during interpreting.
data InterpError where

  -- | An unbound name.
  UnboundError  :: Name Core -> InterpError

  -- | v should be a number, but isn't.
  NotANum       :: Value     -> InterpError

  -- | Division by zero.
  DivByZero     ::              InterpError

  -- | Underflow, e.g. (2 - 3 : Nat)
  Underflow     ::              InterpError

  -- | Taking the base-2 logarithm of zero.
  LgOfZero      ::              InterpError

  -- | v should be a boolean, but isn't.
  NotABool      :: Value     -> InterpError

  -- | Non-exhaustive case analysis.
  NonExhaustive ::              InterpError

  -- | Internal error for features not yet implemented.
  Unimplemented :: String    -> InterpError

  deriving Show

------------------------------------------------------------
-- Disco monad state
------------------------------------------------------------

-- | A location in memory is represented by an @Int@.
type Loc = Int

-- | The various pieces of state tracked by the 'Disco' monad.
data DiscoState = DiscoState
  {
    _topCtx   :: Ctx Term Type
    -- ^ Top-level type environment.

  , _topDefns :: Ctx Core Core
    -- ^ Environment of top-level definitions.

  , _topDocs  :: Ctx Term Docs
    -- ^ Top-level documentation.

  , _memory   :: IntMap Value
    -- ^ A memory is a mapping from "locations" (uniquely generated
    --   identifiers) to values.  It also keeps track of the next
    --   unused location.  We keep track of a memory during
    --   evaluation, and can create new memory locations to store
    --   things that should only be evaluated once.

  , _nextLoc  :: Loc
    -- ^ The next available (unused) memory location.
  }

-- | The initial state for the @Disco@ monad.
initDiscoState :: DiscoState
initDiscoState = DiscoState
  { _topCtx   = emptyCtx
  , _topDefns = emptyCtx
  , _topDocs  = emptyCtx
  , _memory   = IntMap.empty
  , _nextLoc  = 0
  }

------------------------------------------------------------
-- Disco monad
------------------------------------------------------------

-- | The main monad used by the Disco REPL, and by the interpreter and
--   pretty printer.
--
--   * Keeps track of state such as current top-level definitions,
--     types, and documentation, and memory for allocation of thunks.
--   * Keeps track of a read-only environment for binding local
--     variables to their values.
--   * Can throw 'InterpError' exceptions
--   * Can generate fresh names
--   * Can do I/O
type Disco = StateT DiscoState (ReaderT Env (ExceptT InterpError (LFreshMT IO)))


------------------------------------------------------------
-- Some instances needed to ensure that Disco is an instance of the
-- MonadException class from haskeline.

-- Orphan instance, since it doesn't really seem sensible for either
-- unbound-generics or haskeline to depend on the other.
instance MonadException m => MonadException (LFreshMT m) where
  controlIO f = LFreshMT $ controlIO $ \(RunIO run) -> let
                  run' = RunIO (fmap LFreshMT . run . unLFreshMT)
                  in unLFreshMT <$> f run'


-- We need this orphan instance too.  It would seem to make sense for
-- haskeline to provide an instance for ExceptT, but see:
--   https://github.com/judah/haskeline/issues/18
--   https://github.com/judah/haskeline/pull/22
--
-- Idris does the same thing,
--   https://github.com/idris-lang/Idris-dev/blob/5d7388bb3c71fe56b7c09c0b31a94d44bf9f4f25/src/Idris/Output.hs#L38

instance MonadException m => MonadException (ExceptT e m) where
  controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                  run' = RunIO (fmap ExceptT . run . runExceptT)
                  in runExceptT <$> f run'

------------------------------------------------------------
-- Lenses for state
------------------------------------------------------------

makeLenses ''DiscoState

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | Run a computation in the @Disco@ monad, starting in the empty
--   environment.
runDisco :: Disco a -> IO (Either InterpError a)
runDisco = runLFreshMT . runExceptT . flip runReaderT emptyCtx . flip evalStateT initDiscoState

-- | Allocate a new memory cell for the given value, and return its
--   'Loc'.
allocate :: Value -> Disco Loc
allocate v = do
  loc <- nextLoc <+= 1
  memory %= IntMap.insert loc v
  return loc

-- | Delay a @Disco Value@ computation by packaging it into a @VDelay@
--   constructor.  When it is evaluated later, it will be run with the
--   environment that was current at the time 'delay' was called,
--   /not/ the one that is in effect later.
delay :: Disco Value -> Disco Value
delay imv = do
  e <- getEnv
  return (VDelay . ValDelay $ withEnv e imv)

-- | Create a thunk by packaging up a @Core@ expression with the
--   current environment.  The thunk is stored in a new location in
--   memory, and the returned value consists of an indirection
--   referring to its location.
mkThunk :: Core -> Disco Value
mkThunk c = VIndir <$> (allocate =<< (VThunk c <$> getEnv))
