module Unify where

import Data.Coerce
import Control.Lens (anyOf)

import Unbound.Generics.LocallyNameless

import Types
import Subst
import Constraints

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible).
--
--   This is not the most efficient way to implement unification but
--   it is simple.
unify :: [Eqn Type] -> Maybe S
unify = unify' (==)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations equal *up to* identifying all base
--   types.  So, for example, Int = Nat weakly unifies but Int = (Int
--   -> Int) does not.  This is used to check whether subtyping
--   constraints are structurally sound before doing constraint
--   simplification/solving.
weakUnify :: [Eqn Type] -> Maybe S
weakUnify = unify' (\_ _ -> True)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible), up to the given comparison on base types.
unify' :: (Atom -> Atom -> Bool) -> [Eqn Type] -> Maybe S
unify' _ [] = Just idS
unify' atomEq (e:es) = do
  u <- unifyOne atomEq e
  case u of
    Left sub    -> (@@ sub) <$> unify' atomEq (substs sub es)
    Right newEs -> unify' atomEq (newEs ++ es)

equate :: [Type] -> Maybe S
equate tys = unify eqns
  where
    eqns = zipWith (:=:) tys (tail tys)

occurs :: Name Type -> Type -> Bool
occurs x = anyOf fv (==x)

unifyOne :: (Atom -> Atom -> Bool) -> Eqn Type -> Maybe (Either S [Eqn Type])
unifyOne _ (ty1 :=: ty2)
  | ty1 == ty2 = return $ Left idS
unifyOne _ (TyVar x :=: ty2)
  | occurs x ty2 = Nothing
  | otherwise    = Just $ Left (x |-> ty2)
unifyOne atomEq (ty1 :=: x@(TyVar _))
  = unifyOne atomEq (x :=: ty1)
unifyOne _ (TyCons c1 tys1 :=: TyCons c2 tys2)
  | c1 == c2  = return $ Right (zipWith (:=:) tys1 tys2)
  | otherwise = Nothing
unifyOne atomEq (TyAtom a1 :=: TyAtom a2)
  | atomEq a1 a2 = return $ Left idS
  | otherwise    = Nothing
unifyOne _ _ = Nothing  -- Atom = Cons

unifyAtoms :: [Atom] -> Maybe (S' Atom)
unifyAtoms = fmap convert . equate . map TyAtom
  where
    -- Guaranteed that this will get everything in the list, since we
    -- started with all atoms.
    convert s = [(coerce x, a) | (x, TyAtom a) <- s]
