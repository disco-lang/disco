-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Unify
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Unification.
--
-----------------------------------------------------------------------------

module Disco.Typecheck.Unify where

import           Unbound.Generics.LocallyNameless (Name, fv, substs)

import           Control.Lens                     (anyOf)
import           Data.Coerce

import           Disco.Subst
import           Disco.Types

-- XXX todo: might be better if unification took sorts into account
-- directly.  As it is, however, I think it works properly;
-- e.g. suppose we have a with sort {sub} and we unify it with Bool.
-- unify will just return a substitution [a |-> Bool].  But then when
-- we call extendSubst, and in particular substSortMap, the sort {sub}
-- will be applied to Bool and decomposed which will throw an error.

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible).
--
--   This is not the most efficient way to implement unification but
--   it is simple.
unify :: [(Type, Type)] -> Maybe S
unify = unify' (==)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations equal *up to* identifying all base
--   types.  So, for example, Int = Nat weakly unifies but Int = (Int
--   -> Int) does not.  This is used to check whether subtyping
--   constraints are structurally sound before doing constraint
--   simplification/solving.
weakUnify :: [(Type, Type)] -> Maybe S
weakUnify = unify' (\_ _ -> True)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible), up to the given comparison on base types.
unify' :: (BaseTy -> BaseTy -> Bool) -> [(Type, Type)] -> Maybe S
unify' _ [] = Just idS
unify' baseEq (e:es) = do
  u <- unifyOne baseEq e
  case u of
    Left sub    -> (@@ sub) <$> unify' baseEq (substs sub es)
    Right newEs -> unify' baseEq (newEs ++ es)

equate :: [Type] -> Maybe S
equate tys = unify eqns
  where
    eqns = zipWith (,) tys (tail tys)

occurs :: Name Type -> Type -> Bool
occurs x = anyOf fv (==x)

unifyOne :: (BaseTy -> BaseTy -> Bool) -> (Type, Type) -> Maybe (Either S [(Type, Type)])

unifyOne _ (ty1, ty2)
  | ty1 == ty2 = return $ Left idS

unifyOne _ (TyVar x, ty2)
  | occurs x ty2 = Nothing
  | otherwise    = Just $ Left (x |-> ty2)
unifyOne baseEq (ty1, x@(TyVar _))
  = unifyOne baseEq (x, ty1)

-- At this point we know ty2 isn't the same skolem nor a unification variable.
-- Skolems don't unify with anything.
unifyOne _ (Skolem _, _) = Nothing
unifyOne _ (_, Skolem _) = Nothing

unifyOne _ (TyCon c1 tys1, TyCon c2 tys2)
  | c1 == c2  = return $ Right (zipWith (,) tys1 tys2)
  | otherwise = Nothing
unifyOne baseEq (TyAtom (ABase b1), TyAtom (ABase b2))
  | baseEq b1 b2 = return $ Left idS
  | otherwise    = Nothing
unifyOne _ _ = Nothing  -- Atom = Cons

unifyAtoms :: [Atom] -> Maybe (S' Atom)
unifyAtoms = fmap convert . equate . map TyAtom
  where
    -- Guaranteed that this will get everything in the list, since we
    -- started with all atoms.
    convert s = [(coerce x, a) | (x, TyAtom a) <- s]
