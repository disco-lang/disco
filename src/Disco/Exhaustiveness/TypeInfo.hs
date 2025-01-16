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

instance Eq TypedVar where
  TypedVar (n1, _) == TypedVar (n2, _) = n1 == n2

getType :: TypedVar -> Ty.Type
getType (TypedVar (_, t)) = t

data DataCon = DataCon
  { dcIdent :: Ident,
    dcTypes :: [Ty.Type]
  }
  deriving (Ord, Show)

-- This is very important, as we have (sometimes recursive) type aliases
-- Equality of dcTypes doesn't measure equality of dataconstructors,
-- because we could have two different aliases for the same type
-- (And we can't just resolve all aliases up front, because they can be recursive)
instance Eq DataCon where
  (==) = (==) `on` dcIdent

data Ident where
  KUnit :: Ident
  KBool :: Bool -> Ident
  KNat :: Integer -> Ident
  KInt :: Integer -> Ident
  KPair :: Ident
  KCons :: Ident
  KNil :: Ident
  KChar :: Char -> Ident
  KLeft :: Ident
  KRight :: Ident
  KUnkown :: Ident
  deriving (Eq, Ord)

instance Show Ident where
  show i = case i of
    KBool b -> show b
    KChar c -> show c
    KNat n -> show n
    KInt z ->
      if z < 0
        then "(" ++ show z ++ ")"
        else show z
    KNil -> "[]"
    KUnit -> "unit"
    KUnkown -> "_"
    -- These should never actually be printed in warnings
    KPair -> ","
    KCons -> "::"
    KLeft -> "left()"
    KRight -> "right()"

-- | 'Finite' constructors are used in the LYG checker
--   'Infinite' constructors are used when reporting
--   examples of uncovered patterns, we only pick out a few of them
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

integer :: Integer -> DataCon
integer z = DataCon {dcIdent = KInt z, dcTypes = []}

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

-- TODO(colin): ask yorgey, make sure I've done this correctly
-- If I have, and this is enough, I can remove all mentions
-- of type equality constraints in Constraint.hs,
-- the lookup here will have handled that behavoir already
--
-- Type aliases that would cause infinite recursion here are
-- not possible to construct, so we don't have to worry about that.
-- (aka cyclic type definitions are not allowed in Disco)
resolveAlias :: Ty.Type -> Ty.TyDefCtx -> Ty.Type
resolveAlias (Ty.TyUser name args) ctx = case M.lookup name ctx of
  Nothing -> error $ show ctx ++ "\nType definition not found for: " ++ show name
  Just (Ty.TyDefBody _argNames typeCon) -> resolveAlias (typeCon args) ctx
resolveAlias t _ = t

-- TODO(colin): ask yorgey to see if I've handled all the thing
-- I need to here. The list of things caught by the wildcard is in the comment below the function
tyDataConsHelper :: Ty.Type -> Constructors
tyDataConsHelper (a Ty.:*: b) = Finite [pair a b]
tyDataConsHelper (l Ty.:+: r) = Finite [left l, right r]
tyDataConsHelper t@(Ty.TyList a) = Finite [nil, cons a t]
tyDataConsHelper Ty.TyVoid = Finite []
tyDataConsHelper Ty.TyUnit = Finite [unit]
tyDataConsHelper Ty.TyBool = Finite [bool True, bool False]
tyDataConsHelper Ty.TyN = Infinite $ map natural [0, 1 ..]
-- Many thanks to this answer and its comment for a convenient way to list the integers
-- https://stackoverflow.com/a/9749957
tyDataConsHelper Ty.TyZ = Infinite $ map integer $ 0 : [y | x <- [1 ..], y <- [x, -x]]
tyDataConsHelper Ty.TyF = Infinite []
tyDataConsHelper Ty.TyQ = Infinite []
-- TODO(colin): We could do all valid ASCII, but this is most likely good enough
-- I think these starting from 'a' is good for students learning the language
tyDataConsHelper Ty.TyC =
  Infinite $
    map char $
      ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
tyDataConsHelper _ = Infinite [unknown]
-- ^ This includes:
-- (_ Ty.:->: _) (Ty.TySet _) (Ty.TyBag _) (Ty.TyVar _)
-- (Ty.TySkolem _) (Ty.TyProp) (Ty.TyMap _ _) (Ty.TyGraph _)
-- TODO(colin): confim below:
-- I think all of these are impossible to pattern match against
-- with anything other than a wildcard.
-- So they should be always fully covered. 
-- But if they are in a pair, like a Set(Int)*Int,
-- We still need to generate 3 examples of the pair if that Int
-- part isn't covered.
-- So how do we fill the concrete part of Set(Int)?
-- For now I'm calling that unknown, and printing an underscore
-- I believe this also applies when pattern matching 'Maybe a' types
-- We need stand in for an example of a concrete 'a'
--
-- iirc, these are 'Infinite' because the only way to match against
-- them is with a wildcard or variable pattern, and marking them 'Infinite'
-- conveys essentially just that to the LYG checker
--
-- Maybe this should be rewritten in a big doc comment above?

newName :: (Member Fresh r) => Sem r (Name ATerm)
newName = fresh $ s2n ""

newVar :: (Member Fresh r) => Ty.Type -> Sem r TypedVar
newVar types = do
  names <- newName
  return $ TypedVar (names, types)

newNames :: (Member Fresh r) => Int -> Sem r [Name ATerm]
newNames i = replicateM i newName

newVars :: (Member Fresh r) => [Ty.Type] -> Sem r [TypedVar]
newVars types = do
  names <- newNames (length types)
  return $ zipWith (curry TypedVar) names types
