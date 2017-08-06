{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE TemplateHaskell          #-}

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

       , Env
       , Loc, Memory(..), memStore, nextLoc, initMemory

         -- * Errors

       , InterpError(..)

         -- * Disco monad

       , IM, runIM
       , emptyEnv, extend, extends, getEnv, withEnv
       , allocate
       , delay, mkThunk

       )
       where

import           Control.Lens                       ((<+=), (%=), makeLenses)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.IntMap.Lazy                   (IntMap)
import qualified Data.IntMap.Lazy                   as IntMap

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Core

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

  -- | A delayed value, containing an @IM Value@ computation which can
  --   be run later.
  VDelay  :: ValDelay -> Value
  deriving Show

-- | A @ValFun@ is just a Haskell function @Value -> Value@.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValFun = ValFun (Value -> Value)

instance Show ValFun where
  show _ = "<fun>"

-- | A @ValDelay@ is just an @IM Value@ computation.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValDelay = ValDelay (IM Value)

instance Show ValDelay where
  show _ = "<delay>"

------------------------------------------------------------
-- Environments
------------------------------------------------------------

type Loc = Int

-- | An environment is a mapping from names to values.
type Env  = Map (Name Core) Value

-- | A memory is a mapping from "locations" (uniquely generated
--   identifiers) to values.  It also keeps track of the next
--   unused location.  We keep track of a memory during evaluation,
--   and can create new memory locations to store things that should
--   only be evaluated once.
data Memory = Memory { _memStore :: IntMap Value, _nextLoc :: Loc }

initMemory :: Memory
initMemory = Memory IntMap.empty 0

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
-- Interpreter monad
------------------------------------------------------------

-- | The interpreter monad.  Combines read-only access to an
--   environment, and the ability to throw @InterpErrors@ and generate
--   fresh names.
type IM = StateT Memory (ReaderT Env (ExceptT InterpError LFreshM))

makeLenses ''Memory

-- | Run a computation in the @IM@ monad, starting in the empty
--   environment.
runIM :: IM a -> Either InterpError a
runIM = runLFreshM . runExceptT . flip runReaderT M.empty . flip evalStateT initMemory

-- | The empty environment.
emptyEnv :: Env
emptyEnv = M.empty

-- | Locally extend the environment with a new name -> value mapping,
--   (shadowing any existing binding for the given name).
extend :: Name Core -> Value -> IM a -> IM a
extend x v = avoid [AnyName x] . local (M.insert x v)

-- | Locally extend the environment with another environment.
--   Bindings in the new environment shadow bindings in the old.
extends :: Env -> IM a -> IM a
extends e' = avoid (map AnyName (M.keys e')) . local (M.union e')

-- | Get the current environment.
getEnv :: IM Env
getEnv = ask

-- | Run an @IM@ computation with a /replaced/ (not extended)
--   environment.  This is used for evaluating things such as closures
--   and thunks that come with their own environment.
withEnv :: Env -> IM a -> IM a
withEnv = local . const

-- | Allocate a new memory cell for the given value, and return its
--   'Loc'.
allocate :: Value -> IM Loc
allocate v = do
  loc <- nextLoc <+= 1
  memStore %= IntMap.insert loc v
  return loc

-- | Delay an @IM Value@ computation by packaging it into a @VDelay@
--   constructor.  When it is evaluated later, it will be run with the
--   environment that was current at the time 'delay' was called,
--   /not/ the one that is in effect later.
delay :: IM Value -> IM Value
delay imv = do
  e <- getEnv
  return (VDelay . ValDelay $ withEnv e imv)

-- | Create a thunk by packaging up a @Core@ expression with the
--   current environment.  The thunk is stored in a new location in
--   memory, and the returned value consists of an indirection
--   referring to its location.
mkThunk :: Core -> IM Value
mkThunk c = VIndir <$> (allocate =<< (VThunk c <$> getEnv))
