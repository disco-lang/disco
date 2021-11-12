
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

    Value(.., VNil, VCons, VFun)
  , SimpleValue(..)
  , toSimpleValue, fromSimpleValue

    -- ** Conversion

  , ratv, vrat
  , intv, vint
  , charv, vchar
  , enumv
  , pairv, vpair
  , listv, vlist

    -- * Props & testing
  , ValProp(..), TestResult(..), TestReason_(..), TestReason
  , SearchType(..), SearchMotive(.., SMExists, SMForall)
  , TestVars(..), TestEnv(..), emptyTestEnv, getTestEnv, extendPropEnv, extendResultEnv

  -- * Environments

  , Env, extendEnv, extendsEnv, getEnv, withEnv

  -- * Memory
  , Cell(..), Mem, emptyMem, allocate, allocateRec, lkup, set

  -- * Evaluation effects

  , Debug(..)
  , debug
  , EvalEffects

  ) where

import           Control.Monad                    (forM)
import           Data.Bifunctor                   (first)
import           Data.Char                        (chr, ord)
import           Data.IntMap                      (IntMap)
import qualified Data.IntMap                      as IM
import           Data.List                        (foldl')
import           Data.Map                         (Map)
import           Data.Ratio

import           Algebra.Graph                    (Graph)

import           Disco.AST.Core
import           Disco.AST.Generic                (Side (..))
import           Disco.Context                    as Ctx
import           Disco.Error
import           Disco.Names
import           Disco.Types

import           Disco.Effects.LFresh
import           Disco.Effects.Random
import           Polysemy
import           Polysemy.Error
import           Polysemy.Fail
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State
import           Unbound.Generics.LocallyNameless (AnyName (..), Name)

------------------------------------------------------------
-- Evaluation effects
------------------------------------------------------------

newtype Debug = Debug { unDebug :: String }

debug :: Member (Output Debug) r => String -> Sem r ()
debug = output . Debug

--- Get rid of Reader Env --- should be dispatched locally?
type EvalEffects = [Reader Env, Fail, Error EvalError, Random, LFresh, Output Debug, State Mem]
  -- XXX write about order.
  -- memory, counter etc. should not be reset by errors.

  -- XXX add some kind of proper logging effect(s)
    -- With tags so we can filter on log messages we want??

------------------------------------------------------------
-- Value type
------------------------------------------------------------

-- | Different types of values which can result from the evaluation
--   process.
data Value where

  -- | A numeric value, which also carries a flag saying how
  --   fractional values should be diplayed.
  VNum     :: RationalDisplay -> Rational -> Value

  -- | A built-in function constant.
  VConst   :: Op -> Value

  -- | An injection into a sum type.
  VInj     :: Side -> Value -> Value

  -- | The unit value.
  VUnit    :: Value

  -- | A pair of values.
  VPair    :: Value -> Value -> Value

  -- | A closure, i.e. a function body together with its
  --   environment.
  VClo     :: Env -> [Name Core] -> Core -> Value

  -- | A disco type can be a value.  For now, there are only a very
  --   limited number of places this could ever show up (in
  --   particular, as an argument to @enumerate@ or @count@).
  VType    :: Type -> Value

  -- | A reference, i.e. a pointer to a memory cell.  This is used to
  --   implement (optional, user-requested) laziness as well as
  --   recursion.
  VRef     :: Int -> Value

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

toSimpleValue :: Value -> SimpleValue
toSimpleValue = \case
  VNum d n    -> SNum d n
  VUnit       -> SUnit
  VInj s v1   -> SInj s (toSimpleValue v1)
  VPair v1 v2 -> SPair (toSimpleValue v1) (toSimpleValue v2)
  VBag bs     -> SBag (map (first toSimpleValue) bs)
  VType t     -> SType t
  t           -> error $ "A non-simple value was passed as simple" ++ show t

fromSimpleValue :: SimpleValue -> Value
fromSimpleValue (SNum d n)    = VNum d n
fromSimpleValue SUnit         = VUnit
fromSimpleValue (SInj s v)    = VInj s (fromSimpleValue v)
fromSimpleValue (SPair v1 v2) = VPair (fromSimpleValue v1) (fromSimpleValue v2)
fromSimpleValue (SBag bs)     = VBag $ map (first fromSimpleValue) bs
fromSimpleValue (SType t)     = VType t

-- | A @ValFun@ is just a Haskell function @Value -> Value@.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValFun = ValFun (Value -> Value)

instance Show ValFun where
  show _ = "<fun>"

pattern VFun :: (Value -> Value) -> Value
pattern VFun f = VFun_ (ValFun f)

------------------------------------------------------------
-- Converting to and from Value
------------------------------------------------------------

-- XXX write some comments about partiality

-- | A convenience function for creating a default @VNum@ value with a
--   default (@Fractional@) flag.
ratv :: Rational -> Value
ratv = VNum mempty

vrat :: Value -> Rational
vrat (VNum _ r) = r
vrat v          = error $ "vrat " ++ show v

-- | A convenience function for creating a default @VNum@ value with a
--   default (@Fractional@) flag.
intv :: Integer -> Value
intv = ratv . (% 1)

vint :: Value -> Integer
vint (VNum _ n) = numerator n
vint v          = error $ "vint " ++ show v

vchar :: Value -> Char
vchar = chr . fromIntegral . vint

charv :: Char -> Value
charv = intv . fromIntegral . ord

-- | Turn any instance of @Enum@ into a @Value@, by creating a
--   constructor with an index corresponding to the enum value.
enumv :: Enum e => e -> Value
enumv e = VInj (toEnum $ fromEnum e) VUnit

pairv :: (a -> Value) -> (b -> Value) -> (a,b) -> Value
pairv av bv (a,b) = VPair (av a) (bv b)

vpair :: (Value -> a) -> (Value -> b) -> Value -> (a,b)
vpair va vb (VPair a b) = (va a, vb b)
vpair _ _ v             = error $ "vpair " ++ show v

listv :: (a -> Value) -> [a] -> Value
listv _ []        = VNil
listv eltv (a:as) = VCons (eltv a) (listv eltv as)

vlist :: (Value -> a) -> Value -> [a]
vlist _ VNil            = []
vlist velt (VCons v vs) = velt v : vlist velt vs
vlist _ v               = error $ "vlist " ++ show v


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

getTestEnv :: Members '[Error EvalError] r => TestVars -> Env -> Sem r TestEnv
getTestEnv (TestVars tvs) e = fmap TestEnv . forM tvs $ \(s, ty, name) -> do
  let value = Ctx.lookup' (localName name) e
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
extendEnv x v = avoid [AnyName x] . extend (localName x) v

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
-- Memory
------------------------------------------------------------

------------------------------------------------------------
-- Memory
------------------------------------------------------------

-- | 'Mem' represents a memory, containing 'Cell's
data Mem = Mem { next :: Int, mu :: IntMap Cell } deriving Show
data Cell = Blackhole | E Env Core | V Value deriving Show

emptyMem :: Mem
emptyMem = Mem 0 IM.empty

-- | Allocate a new memory cell containing an unevaluated expression
--   with the current environment.  Return the index of the allocated
--   cell.
allocate :: Members '[State Mem] r => Env -> Core -> Sem r Int
allocate e t = do
  Mem n m <- get
  put $ Mem (n+1) (IM.insert n (E e t) m)
  return n

-- | Allocate new memory cells for a group of mutually recursive
--   bindings, and return the indices of the allocate cells.
allocateRec :: Members '[State Mem] r => Env -> [(QName Core, Core)] -> Sem r [Int]
allocateRec e bs = do
  Mem n m <- get
  let newRefs = zip [n ..] bs
      e' = foldl' (flip (\(i,(x,_)) -> Ctx.insert x (VRef i))) e newRefs
      m' = foldl' (flip (\(i,(_,c)) -> IM.insert i (E e' c))) m newRefs
      n' = n + length bs
  put $ Mem n' m'
  return [n .. n'-1]

-- | Look up the cell at a given index.
lkup :: Members '[State Mem] r => Int -> Sem r (Maybe Cell)
lkup n = gets (IM.lookup n . mu)

-- | Set the cell at a given index.
set :: Members '[State Mem] r => Int -> Cell -> Sem r ()
set n c = modify $ \(Mem nxt m) -> Mem nxt (IM.insert n c m)
