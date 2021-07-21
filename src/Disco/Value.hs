{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Value
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Disco runtime values and environments.
--
-----------------------------------------------------------------------------

module Disco.Value
  ( -- * Values

    Value(.., VFun, VDelay), pattern VNil, pattern VCons
  , SimpleValue(..)

    -- * Props & testing
  , ValProp(..), TestResult(..), TestReason_(..), TestReason
  , SearchType(..), SearchMotive(.., SMExists, SMForall)
  , TestVars(..), TestEnv(..), emptyTestEnv, getTestEnv, extendPropEnv, extendResultEnv

  -- * Environments

  , Env, extendEnv, extendsEnv, getEnv, withEnv

  -- * Memory store

  , Cell(..), mkCell, Loc

  -- * Evaluation effects

  , Debug(..)
  , debug
  , EvalEffects

  ) where

import           Data.IntSet                      (IntSet)
import           Data.Map                         (Map)
import qualified Data.Map                         as M

import           Algebra.Graph                    (Graph)

import           Control.Monad                    (forM)
import           Disco.AST.Core
import           Disco.AST.Generic                (Side (..))
import           Disco.Context
import           Disco.Error
import           Disco.Types

import           Disco.Effects.LFresh
import           Disco.Effects.Random
import           Disco.Effects.Store
import           Polysemy
import           Polysemy.Error
import           Polysemy.Fail
import           Polysemy.Output
import           Polysemy.Reader
import           Unbound.Generics.LocallyNameless (AnyName (..), Bind, Name)

------------------------------------------------------------
-- Evaluation effects
------------------------------------------------------------

newtype Debug = Debug { unDebug :: String }

debug :: Member (Output Debug) r => String -> Sem r ()
debug = output . Debug

-- Get rid of Reader Env --- should be dispatched locally?
type EvalEffects = [Reader Env, Fail, Error EvalError, Store Cell, Random, LFresh, Output Debug]
  -- XXX write about order.
  -- memory, counter etc. should not be reset by errors.

  -- XXX add some kind of proper logging effect(s)
    -- With tags so we can filter on log messages we want??
    -- Just make my own message logging effect.

------------------------------------------------------------
-- Values
------------------------------------------------------------

-- | The type of values produced by the interpreter. The parameter @r@
--   is an effect row listing the effects needed to evaluate such values.
data Value where
  -- | A numeric value, which also carries a flag saying how
  --   fractional values should be diplayed.
  VNum  :: RationalDisplay -> Rational -> Value

  -- | The unit value.
  VUnit :: Value

  -- | An injection into a sum type.
  VInj :: Side -> Value -> Value

  -- | A pair of values.
  VPair :: Value -> Value -> Value

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
  VIndir :: Int -> Value

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

  -- | A @Value@ computation which can be run later, along with
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
  VMap :: Map SimpleValue Value -> Value

  -- | A disco type can be a value.  For now, there are only a very
  --   limited number of places this could ever show up (in
  --   particular, as an argument to @enumerate@ or @count@).
  VType :: Type -> Value
  deriving Show

-- | Convenient pattern for the empty list.
pattern VNil :: Value
pattern VNil      = VInj L VUnit

-- | Convenient pattern for list cons.
pattern VCons :: Value -> Value -> Value
pattern VCons h t = VInj R (VPair h t)

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
  SUnit  :: SimpleValue
  SInj   :: Side -> SimpleValue -> SimpleValue
  SPair  :: SimpleValue -> SimpleValue -> SimpleValue
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

-- | A @ValDelay@ is just a @Sem r Value@ computation.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValDelay = ValDelay (forall r. Members EvalEffects r => Sem r Value)

instance Show ValDelay where
  show _ = "<delay>"

pattern VDelay
  :: (forall r. Members EvalEffects r => Sem r Value)
  -> IntSet -> Env -> Value
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
  deriving newtype (Show, Semigroup, Monoid)

-- | A variable assignment found during a test.
newtype TestEnv = TestEnv [(String, Type, Value)]
  deriving newtype (Show, Semigroup, Monoid)

emptyTestEnv :: TestEnv
emptyTestEnv = TestEnv []

getTestEnv :: Members '[Reader Env, Error EvalError] r => TestVars -> Sem r TestEnv
getTestEnv (TestVars tvs) = fmap TestEnv . forM tvs $ \(s, ty, name) -> do
  value <- M.lookup name <$> getEnv
  case value of
    Just v  -> return (s, ty, v)
    Nothing -> throw (UnboundError name)

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
  | TestRuntimeError EvalError
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
extendEnv :: Members '[Reader Env, LFresh] r => Name Core -> Value -> Sem r a -> Sem r a
extendEnv x v = avoid [AnyName x] . extend x v

-- | Locally extend the environment with another environment.
--   Bindings in the new environment shadow bindings in the old.
extendsEnv :: Members '[Reader Env, LFresh] r => Env -> Sem r a -> Sem r a
extendsEnv e' = avoid (map AnyName (names e')) . extends e'

-- | Get the current environment.
getEnv :: Member (Reader Env) r => Sem r Env
getEnv = ask

-- | Run a @Disco@ computation with a /replaced/ (not extended)
--   environment.  This is used for evaluating things such as closures
--   and thunks that come with their own environment.
withEnv :: Members '[Reader Env, LFresh] r => Env -> Sem r a -> Sem r a
withEnv e = avoid (map AnyName (names e)) . local (const e)

-- The below code seems to work too, but I don't understand why.  Seems like
-- we should have to avoid names bound in the environment currently in use.

-- withEnv = local . const


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
