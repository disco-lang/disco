-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Unify
-- Copyright   :  (c) 2018 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Unification.
--
-----------------------------------------------------------------------------

module Disco.Typecheck.Unify where

import           Unbound.Generics.LocallyNameless (Name, fv, substs)

import           Control.Lens                     (anyOf)
import           Control.Monad.State
import           Data.Coerce
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Set                         (Set)
import qualified Data.Set                         as S

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
unify :: Map String Type -> [(Type, Type)] -> Maybe S
unify = unify' (==)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations equal *up to* identifying all base
--   types.  So, for example, Int = Nat weakly unifies but Int = (Int
--   -> Int) does not.  This is used to check whether subtyping
--   constraints are structurally sound before doing constraint
--   simplification/solving.
weakUnify :: Map String Type -> [(Type, Type)] -> Maybe S
weakUnify = unify' (\_ _ -> True)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible), up to the given comparison on base types.
unify' :: (BaseTy -> BaseTy -> Bool) -> M.Map String Type -> [(Type, Type)] -> Maybe S
unify' baseEq tyDefns eqs = evalStateT (go eqs) S.empty
  where
    go :: [(Type, Type)] -> StateT (Set (Type,Type)) Maybe S
    go [] = return idS
    go (e:es) = do
      u <- unifyOne e
      case u of
        Left sub    -> (@@ sub) <$> go (substs sub es)
        Right newEs -> go (newEs ++ es)

    unifyOne :: (Type, Type) -> StateT (Set (Type,Type)) Maybe (Either S [(Type, Type)])
    unifyOne pair = do
      seen <- get
      case pair `S.member` seen of
        True  -> return $ Left idS
        False -> unifyOne' pair

    unifyOne' :: (Type, Type) -> StateT (Set (Type,Type)) Maybe (Either S [(Type, Type)])

    unifyOne' (ty1, ty2)
      | ty1 == ty2 = return $ Left idS

    unifyOne' (TyVar x, ty2)
      | occurs x ty2 = mzero
      | otherwise    = return $ Left (x |-> ty2)
    unifyOne' (ty1, x@(TyVar _))
      = unifyOne (x, ty1)

    -- At this point we know ty2 isn't the same skolem nor a unification variable.
    -- Skolems don't unify with anything.
    unifyOne' (Skolem _, _) = mzero
    unifyOne' (_, Skolem _) = mzero

    unifyOne' p@(TyAdt t, ty2) = do
      modify (S.insert p)
      case M.lookup t tyDefns of
        Nothing  -> mzero
        Just ty1 -> return $ Right [(ty1,ty2)]

    unifyOne' p@(ty1, TyAdt t) = do
      modify (S.insert p)
      case M.lookup t tyDefns of
        Nothing  -> mzero
        Just ty2 -> return $ Right [(ty1,ty2)]

    unifyOne' p@(TyCon c1 tys1, TyCon c2 tys2)
      | c1 == c2  = do
          modify (S.insert p)
          return $ Right (zipWith (,) tys1 tys2)
      | otherwise = mzero
    unifyOne' (TyAtom (ABase b1), TyAtom (ABase b2))
      | baseEq b1 b2 = return $ Left idS
      | otherwise    = mzero
    unifyOne' _ = mzero  -- Atom = Cons


equate :: Map String Type -> [Type] -> Maybe S
equate tyDefns tys = unify tyDefns eqns
  where
    eqns = zipWith (,) tys (tail tys)

occurs :: Name Type -> Type -> Bool
occurs x = anyOf fv (==x)


unifyAtoms :: Map String Type -> [Atom] -> Maybe (S' Atom)
unifyAtoms tyDefns = fmap convert . equate tyDefns . map TyAtom
  where
    -- Guaranteed that this will get everything in the list, since we
    -- started with all atoms.
    convert s = [(coerce x, a) | (x, TyAtom a) <- s]
