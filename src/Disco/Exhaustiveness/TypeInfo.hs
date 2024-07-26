module Disco.Exhaustiveness.TypeInfo where

import Control.Monad (replicateM)
import qualified Data.Map as M
import Disco.AST.Typed (ATerm)
import Disco.Effects.Fresh (Fresh, fresh)
import qualified Disco.Types as Ty
import Polysemy
import Unbound.Generics.LocallyNameless (Name, s2n)

newtype TypedVar = TypedVar (Name ATerm, Ty.Type)
  deriving (Show, Ord)

-- For now, equality is always in terms of the name
-- We will see if the causes problems later
instance Eq TypedVar where
  TypedVar (n1, _t1) == TypedVar (n2, _t2) = n1 == n2

getType :: TypedVar -> Ty.Type
getType (TypedVar (_, t)) = t

data DataCon = DataCon
  { dcIdent :: Ident,
    dcTypes :: [Ty.Type]
  }
  deriving (Eq, Ord, Show)

data Ident where
  KUnit :: Ident
  KBool :: Bool -> Ident
  KNat :: Integer -> Ident
  KPair :: Ident
  KCons :: Ident
  KNil :: Ident
  KChar :: Char -> Ident
  KLeft :: Ident
  KRight :: Ident
  KUnkown :: Ident
  deriving (Eq, Ord, Show)

data Constructors where
  Finite :: [DataCon] -> Constructors
  Infinite :: [DataCon] -> Constructors

unknown :: DataCon
unknown = DataCon {dcIdent = KUnkown, dcTypes = []}

unit :: DataCon
unit = DataCon {dcIdent = KUnit, dcTypes = []}

bool :: Bool -> DataCon
bool b = DataCon {dcIdent = KBool b, dcTypes = []}

natural :: Integer -> DataCon
natural n = DataCon {dcIdent = KNat n, dcTypes = []}

char :: Char -> DataCon
char c = DataCon {dcIdent = KChar c, dcTypes = []}

cons :: Ty.Type -> Ty.Type -> DataCon
cons tHead tTail = DataCon {dcIdent = KCons, dcTypes = [tHead, tTail]}

nil :: DataCon
nil = DataCon {dcIdent = KNil, dcTypes = []}

pair :: Ty.Type -> Ty.Type -> DataCon
pair a b = DataCon {dcIdent = KPair, dcTypes = [a, b]}

left :: Ty.Type -> DataCon
left tl = DataCon {dcIdent = KLeft, dcTypes = [tl]}

right :: Ty.Type -> DataCon
right tr = DataCon {dcIdent = KRight, dcTypes = [tr]}

-- TODO(colin): ask yorgey, make sure I've done this correctly
-- If I have, and this is enough, I can remove all mentions
-- of type equality constraints in Constraint.hs,
-- the lookup here will have handled that behavoir already
tyDataCons :: Ty.Type -> Ty.TyDefCtx -> Constructors
tyDataCons (Ty.TyUser name args) ctx = case M.lookup name ctx of
  Nothing -> error $ "Type definition not found for: " ++ show name
  Just (Ty.TyDefBody _argNames typeCon) -> tyDataCons (typeCon args) ctx
tyDataCons (a Ty.:*: b) _ = Finite [pair a b]
tyDataCons (l Ty.:+: r) _ = Finite [left l, right r]
tyDataCons t@(Ty.TyList a) _ = Finite [cons a t, nil]
tyDataCons Ty.TyVoid _ = Finite []
tyDataCons Ty.TyUnit _ = Finite [unit]
tyDataCons Ty.TyBool _ = Finite [bool True, bool False]
tyDataCons Ty.TyN _ = Infinite $ map natural [0,1..]
tyDataCons Ty.TyZ _ = Infinite [] -- TODO(colin): IMPORTANT! fill these in!
tyDataCons Ty.TyF _ = Infinite []
tyDataCons Ty.TyQ _ = Infinite []
tyDataCons Ty.TyC _ = Infinite []
tyDataCons (_ Ty.:->: _) _ = error "Functions not allowed in patterns."
tyDataCons (Ty.TySet _) _ = error "Sets not allowed in patterns."
tyDataCons (Ty.TyBag _) _ = error "Bags not allowed in patterns."
-- This caused a problem in findPosExamples,
-- we were trying to list of the constructors of an unknown type
-- Here we return an Infinite to prevent the lyg algorithm
-- from thinking a complete list of constructors exists,
-- and pretty print this as "_" when displaying the result of findPosExamples
tyDataCons (Ty.TyVar _) _ = Infinite [unknown]
-- Unsure about these...
tyDataCons (Ty.TySkolem _) _ = error "Encountered skolem in pattern"
tyDataCons (Ty.TyProp) _ = error "Propositions not allowed in patterns."
tyDataCons (Ty.TyMap _ _) _ = error "Maps not allowed in patterns."
tyDataCons (Ty.TyGraph _) _ = error "Graph not allowed in patterns."

newName :: (Member Fresh r) => Sem r (Name ATerm)
newName = fresh $ s2n ""

newVar :: (Member Fresh r) => Ty.Type -> Sem r TypedVar
newVar types = do
  names <- newName
  return $ TypedVar $ (names, types)

newNames :: (Member Fresh r) => Int -> Sem r [Name ATerm]
newNames i = replicateM i newName

newVars :: (Member Fresh r) => [Ty.Type] -> Sem r [TypedVar]
newVars types = do
  names <- newNames (length types)
  return $ map TypedVar $ zip names types
