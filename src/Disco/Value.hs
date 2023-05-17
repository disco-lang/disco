{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
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
  , testIsOk, testIsError, testReason, testEnv, resultIsCertain

  , LOp(..), interpLOp

  -- * Environments

  , Env

  -- * Memory
  , Cell(..), Mem, emptyMem, allocate, allocateRec, lkup, set

  -- * Pretty-printing

  , prettyValue', prettyValue
  ) where

import           Prelude                          hiding ((<>))
import qualified Prelude                          as P

import           Control.Monad                    (forM)
import           Data.Bifunctor                   (first)
import           Data.Char                        (chr, ord, toLower)
import           Data.IntMap                      (IntMap)
import qualified Data.IntMap                      as IM
import           Data.List                        (foldl')
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Ratio

import           Algebra.Graph                    (Graph, foldg)

import           Disco.AST.Core
import           Disco.AST.Generic                (Side (..))
import           Disco.Context                    as Ctx
import           Disco.Error
import           Disco.Names
import           Disco.Pretty
import           Disco.Syntax.Operators           (BOp (Add, Mul))
import           Disco.Types

import           Disco.Effects.LFresh
import           Polysemy
import           Polysemy.Input
import           Polysemy.Reader
import           Polysemy.State
import           Unbound.Generics.LocallyNameless (Name)

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

  -- | A graph, stored using an algebraic repesentation.
  VGraph :: Graph SimpleValue -> Value

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
  t           -> error $ "A non-simple value was passed as simple: " ++ show t

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

getTestEnv :: TestVars -> Env -> Either EvalError TestEnv
getTestEnv (TestVars tvs) e = fmap TestEnv . forM tvs $ \(s, ty, name) -> do
  let value = Ctx.lookup' (localName name) e
  case value of
    Just v  -> return (s, ty, v)
    Nothing -> Left (UnboundPanic name)

-- | Binary logical operators.
data LOp = LAnd | LOr | LImpl deriving (Eq, Ord, Show, Enum, Bounded)

interpLOp :: LOp -> Bool -> Bool -> Bool
interpLOp LAnd = (&&)
interpLOp LOr = (||)
interpLOp LImpl = (==>)
  where
    True ==> False = False
    _ ==> _ = True

-- | The possible outcomes of a property test, parametrized over
--   the type of values. A @TestReason@ explains why a proposition
--   succeeded or failed.
data TestReason_ a
  = TestBool
    -- ^ The prop evaluated to a boolean.
  | TestEqual Type a a
    -- ^ The test was an equality test. Records the values being
    --   compared and also their type (which is needed for printing).
  | TestLt Type a a
    -- ^ The test was a less than test. Records the values being
    --   compared and also their type (which is needed for printing).
  | TestNotFound SearchType
    -- ^ The search didn't find any examples/counterexamples.
  | TestFound TestResult
    -- ^ The search found an example/counterexample.
  | TestBin LOp TestResult TestResult
    -- ^ A binary logical operator was used to combine the given two results.
  | TestRuntimeError EvalError
    -- ^ The prop failed at runtime. This is always a failure, no
    --   matter which quantifiers or negations it's under.
  deriving (Show, Functor, Foldable, Traversable)

type TestReason = TestReason_ Value

-- | The possible outcomes of a proposition.
data TestResult = TestResult Bool TestReason TestEnv
  deriving Show

-- | Whether the property test resulted in a runtime error.
testIsError :: TestResult -> Bool
testIsError (TestResult _ (TestRuntimeError _) _) = True
testIsError _                                     = False

-- | Whether the property test resulted in success.
testIsOk :: TestResult -> Bool
testIsOk (TestResult b _ _) = b

-- | The reason the property test had this result.
testReason :: TestResult -> TestReason
testReason (TestResult _ r _) = r

testEnv :: TestResult -> TestEnv
testEnv (TestResult _ _ e) = e

testIsCertain :: TestResult -> Bool
testIsCertain (TestResult _ r _) = resultIsCertain r

resultIsCertain :: TestReason -> Bool
resultIsCertain TestBool                        = True
resultIsCertain TestEqual {}                    = True
resultIsCertain TestLt    {}                    = True
resultIsCertain (TestNotFound Exhaustive)       = True
resultIsCertain (TestNotFound (Randomized _ _)) = False
resultIsCertain (TestFound r)                   = testIsCertain r
resultIsCertain (TestRuntimeError _)            = True
resultIsCertain (TestBin op tr1 tr2)
  | c1 && c2                    = True
  | c1 && ((op == LOr) == ok1)  = True
  | c2 && ((op /= LAnd) == ok2) = True
  | otherwise                   = False
  where
    c1 = testIsCertain tr1
    c2 = testIsCertain tr2
    ok1 = testIsOk tr1
    ok2 = testIsOk tr2

-- | A @ValProp@ is the normal form of a Disco value of type @Prop@.
data ValProp
  = VPDone TestResult
    -- ^ A prop that has already either succeeded or failed.
  | VPSearch SearchMotive [Type] Value TestEnv
    -- ^ A pending search.
  | VPBin LOp ValProp ValProp
    -- ^ A binary logical operator combining two prop values.
  deriving Show

extendPropEnv :: TestEnv -> ValProp -> ValProp
extendPropEnv g (VPDone (TestResult b r e)) = VPDone (TestResult b r (g P.<> e))
extendPropEnv g (VPSearch sm tys v e)       = VPSearch sm tys v (g P.<> e)
extendPropEnv g (VPBin op vp1 vp2)          = VPBin op (extendPropEnv g vp1) (extendPropEnv g vp2)

extendResultEnv :: TestEnv -> TestResult -> TestResult
extendResultEnv g (TestResult b r e) = TestResult b r (g P.<> e)

------------------------------------------------------------
-- Environments
------------------------------------------------------------

-- | An environment is a mapping from names to values.
type Env  = Ctx Core Value

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

------------------------------------------------------------
-- Pretty-printing values
------------------------------------------------------------

prettyValue' :: Member (Input TyDefCtx) r => Type -> Value -> Sem r Doc
prettyValue' ty v = runLFresh . runReader initPA $ prettyValue ty v

prettyValue :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Value -> Sem r Doc

-- Lazily expand any user-defined types
prettyValue (TyUser x args) v = do
  tydefs <- input
  let (TyDefBody _ body) = tydefs M.! x   -- This can't fail if typechecking succeeded
  prettyValue (body args) v

prettyValue _      VUnit                     = "■"
prettyValue TyProp _                         = prettyPlaceholder TyProp
prettyValue TyBool (VInj s _)                = text $ map toLower (show (s == R))
prettyValue TyBool v =
  error $ "Non-VInj passed with Bool type to prettyValue: " ++ show v
prettyValue TyC (vchar -> c)                 = text (show c)
prettyValue (TyList TyC) (vlist vchar -> cs) = doubleQuotes . text . concatMap prettyChar $ cs
  where
    prettyChar = drop 1 . reverse . drop 1 . reverse . show . (:[])
prettyValue (TyList ty) (vlist id -> xs)     = do
  ds <- punctuate (text ",") (map (prettyValue ty) xs)
  brackets (hsep ds)

prettyValue ty@(_ :*: _) v                   = parens (prettyTuple ty v)

prettyValue (ty1 :+: _) (VInj L v)           = "left"  <> prettyVP ty1 v
prettyValue (_ :+: ty2) (VInj R v)           = "right" <> prettyVP ty2 v
prettyValue (_ :+: _) v =
  error $ "Non-VInj passed with sum type to prettyValue: " ++ show v

prettyValue _ (VNum d r)
  | denominator r == 1                       = text $ show (numerator r)
  | otherwise                                = text $ case d of
      Fraction -> show (numerator r) ++ "/" ++ show (denominator r)
      Decimal  -> prettyDecimal r

prettyValue ty@(_ :->: _) _                  = prettyPlaceholder ty

prettyValue (TySet ty) (VBag xs)             = braces $ prettySequence ty "," (map fst xs)
prettyValue (TySet _) v =
  error $ "Non-VBag passed with Set type to prettyValue: " ++ show v
prettyValue (TyBag ty) (VBag xs)             = prettyBag ty xs
prettyValue (TyBag _) v =
  error $ "Non-VBag passed with Bag type to prettyValue: " ++ show v

prettyValue (TyMap tyK tyV) (VMap m)         =
  "map" <> parens (braces (prettySequence (tyK :*: tyV) "," (assocsToValues m)))
  where
    assocsToValues = map (\(k,v) -> VPair (fromSimpleValue k) v) . M.assocs
prettyValue (TyMap _ _) v =
  error $ "Non-map value with map type passed to prettyValue: " ++ show v

prettyValue (TyGraph ty) (VGraph g)          =
  foldg
    "emptyGraph"
    (("vertex" <>) . prettyVP ty . fromSimpleValue)
    (\l r -> withPA (getPA Add) $ lt l <+> "+" <+> rt r)
    (\l r -> withPA (getPA Mul) $ lt l <+> "*" <+> rt r)
    g
prettyValue (TyGraph _) v =
  error $ "Non-graph value with graph type passed to prettyValue: " ++ show v

prettyValue ty@TyAtom{} v =
  error $ "Invalid atomic type passed to prettyValue: " ++ show ty ++ " " ++ show v

prettyValue ty@TyCon{} v =
  error $ "Invalid type constructor passed to prettyValue: " ++ show ty ++ " " ++ show v

-- | Pretty-print a value with guaranteed parentheses.  Do nothing for
--   tuples; add an extra set of parens for other values.
prettyVP :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Value -> Sem r Doc
prettyVP ty@(_ :*: _) = prettyValue ty
prettyVP ty           = parens . prettyValue ty

prettyPlaceholder :: Members '[Reader PA, LFresh] r => Type -> Sem r Doc
prettyPlaceholder ty = "<" <> pretty ty <> ">"

prettyTuple :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Value -> Sem r Doc
prettyTuple (ty1 :*: ty2) (VPair v1 v2) = prettyValue ty1 v1 <> "," <+> prettyTuple ty2 v2
prettyTuple ty v                        = prettyValue ty v

-- | 'prettySequence' pretty-prints a lists of values separated by a delimiter.
prettySequence :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Doc -> [Value] -> Sem r Doc
prettySequence ty del vs = hsep =<< punctuate (return del) (map (prettyValue ty) vs)

-- | Pretty-print a literal bag value.
prettyBag :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> [(Value,Integer)] -> Sem r Doc
prettyBag _ [] = bag empty
prettyBag ty vs
  | all ((==1) . snd) vs = bag $ prettySequence ty "," (map fst vs)
  | otherwise            = bag $ hsep =<< punctuate (return ",") (map prettyCount vs)
  where
    prettyCount (v,1) = prettyValue ty v
    prettyCount (v,n) = prettyValue ty v <+> "#" <+> text (show n)
