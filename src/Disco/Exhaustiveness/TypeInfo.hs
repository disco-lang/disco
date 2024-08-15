module Disco.Exhaustiveness.TypeInfo where

import Control.Monad (replicateM)
import Data.Function (on)
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
  deriving (Ord, Show)

instance Eq DataCon where
  (==) = (==) `on` dcIdent

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

tyDataCons :: Ty.Type -> Ty.TyDefCtx -> Constructors
tyDataCons ty ctx = tyDataConsHelper $ resolveAlias ty ctx

resolveAlias :: Ty.Type -> Ty.TyDefCtx -> Ty.Type
resolveAlias (Ty.TyUser name args) ctx = case M.lookup name ctx of
  Nothing -> error $ show ctx ++ "\nType definition not found for: " ++ show name
  Just (Ty.TyDefBody _argNames typeCon) -> resolveAlias (typeCon args) ctx
resolveAlias t _ = t

-- TODO(colin): ask yorgey, make sure I've done this correctly
-- If I have, and this is enough, I can remove all mentions
-- of type equality constraints in Constraint.hs,
-- the lookup here will have handled that behavoir already
tyDataConsHelper :: Ty.Type -> Constructors
tyDataConsHelper (a Ty.:*: b) = Finite [pair a b]
tyDataConsHelper (l Ty.:+: r) = Finite [left l, right r]
tyDataConsHelper t@(Ty.TyList a) = Finite [nil, cons a t]
tyDataConsHelper Ty.TyVoid = Finite []
tyDataConsHelper Ty.TyUnit = Finite [unit]
tyDataConsHelper Ty.TyBool = Finite [bool True, bool False]
tyDataConsHelper Ty.TyN = Infinite $ map natural [0, 1 ..]
tyDataConsHelper Ty.TyZ = Infinite [] -- TODO(colin): IMPORTANT! fill these in!
tyDataConsHelper Ty.TyF = Infinite []
tyDataConsHelper Ty.TyQ = Infinite []
-- We could do all valid ASCII, but this is most likely good enough
-- I think these starting from 'a' is good for students learning the language
tyDataConsHelper Ty.TyC =
  Infinite $
    map char $
      ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
-- This caused a problem in findPosExamples,
-- we were trying to list of the constructors of an unknown type
-- Here we return an Infinite to prevent the lyg algorithm
-- from thinking a complete list of constructors exists,
-- and pretty print this as "_" when displaying the result of findPosExamples
tyDataConsHelper _ = Infinite [unknown]
-- ^ This includes:
-- tyDataCons (_ Ty.:->: _)
-- tyDataCons (Ty.TySet _)
-- tyDataCons (Ty.TyBag _)
-- tyDataCons (Ty.TyVar _)
-- tyDataCons (Ty.TySkolem _)
-- tyDataCons (Ty.TyProp)
-- tyDataCons (Ty.TyMap _ _)
-- tyDataCons (Ty.TyGraph _)

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
