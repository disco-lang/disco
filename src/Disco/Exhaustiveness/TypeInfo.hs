module Disco.Exhaustiveness.TypeInfo where

import Control.Monad (replicateM)
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
  deriving (Eq, Ord, Show)

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

{-
TODO(colin): Fill out the remaining types here
Remaining:
    TyVar
  , TySkolem
  , TyProp
  , TyBag
  , TySet
  , TyGraph
  , TyMap
  , TyUser
Impossible:
  , (:->:)
-}
tyDataCons :: Ty.Type -> Maybe [DataCon]
tyDataCons (a Ty.:*: b) = Just [pair a b]
tyDataCons (l Ty.:+: r) = Just [left l, right r]
tyDataCons t@(Ty.TyList a) = Just [cons a t, nil]
tyDataCons Ty.TyVoid = Just []
tyDataCons Ty.TyUnit = Just [unit]
tyDataCons Ty.TyBool = Just [bool True, bool False]
tyDataCons Ty.TyN = Nothing
tyDataCons Ty.TyZ = Nothing
tyDataCons Ty.TyF = Nothing
tyDataCons Ty.TyQ = Nothing
tyDataCons Ty.TyC = Nothing

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
