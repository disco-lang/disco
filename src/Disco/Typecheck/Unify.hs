-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Disco.Typecheck.Unify
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Unification.
module Disco.Typecheck.Unify where

import Unbound.Generics.LocallyNameless (Name, fv)

import Control.Lens (anyOf)
import Control.Monad (mzero)
import Control.Monad.State
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Disco.Subst
import Disco.Types

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
unify :: TyDefCtx -> [(Type, Type)] -> Maybe S
unify = unify' (==)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations equal *up to* identifying all base
--   types.  So, for example, Int = Nat weakly unifies but Int = (Int
--   -> Int) does not.  This is used to check whether subtyping
--   constraints are structurally sound before doing constraint
--   simplification/solving, to ensure termination.
weakUnify :: TyDefCtx -> [(Type, Type)] -> Maybe S
weakUnify = unify' (\_ _ -> True)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible), up to the given comparison on base types.
unify' ::
  (BaseTy -> BaseTy -> Bool) ->
  TyDefCtx ->
  [(Type, Type)] ->
  Maybe S
unify' baseEq tyDefns eqs = evalStateT (go eqs) S.empty
 where
  go :: [(Type, Type)] -> StateT (Set (Type, Type)) Maybe S
  go [] = return idS
  go (e : es) = do
    u <- unifyOne e
    case u of
      Left sub -> (@@ sub) <$> go (applySubst sub es)
      Right newEs -> go (newEs ++ es)

  unifyOne :: (Type, Type) -> StateT (Set (Type, Type)) Maybe (Either S [(Type, Type)])
  unifyOne pair = do
    seen <- get
    case pair `S.member` seen of
      True -> return $ Left idS
      False -> unifyOne' pair

  unifyOne' :: (Type, Type) -> StateT (Set (Type, Type)) Maybe (Either S [(Type, Type)])

  unifyOne' (ty1, ty2)
    | ty1 == ty2 = return $ Left idS
  unifyOne' (TyVar x, ty2)
    | occurs x ty2 = mzero
    | otherwise = return $ Left (x |-> ty2)
  unifyOne' (ty1, x@(TyVar _)) =
    unifyOne (x, ty1)
  -- At this point we know ty2 isn't the same skolem nor a unification variable.
  -- Skolems don't unify with anything.
  unifyOne' (TySkolem _, _) = mzero
  unifyOne' (_, TySkolem _) = mzero
  -- Unify two container types: unify the container descriptors as
  -- well as the type arguments
  unifyOne' p@(TyCon (CContainer ctr1) tys1, TyCon (CContainer ctr2) tys2) = do
    modify (S.insert p)
    return $ Right ((TyAtom ctr1, TyAtom ctr2) : zip tys1 tys2)

  -- If one of the types to be unified is a user-defined type,
  -- unfold its definition before continuing the matching
  unifyOne' p@(TyCon (CUser t) tys1, ty2) = do
    modify (S.insert p)
    case M.lookup t tyDefns of
      Nothing -> mzero
      Just (TyDefBody _ body) -> return $ Right [(body tys1, ty2)]
  unifyOne' p@(ty1, TyCon (CUser t) tys2) = do
    modify (S.insert p)
    case M.lookup t tyDefns of
      Nothing -> mzero
      Just (TyDefBody _ body) -> return $ Right [(ty1, body tys2)]

  -- Unify any other pair of type constructor applications: the type
  -- constructors must match exactly
  unifyOne' p@(TyCon c1 tys1, TyCon c2 tys2)
    | c1 == c2 = do
        modify (S.insert p)
        return $ Right (zip tys1 tys2)
    | otherwise = mzero
  unifyOne' (TyAtom (ABase b1), TyAtom (ABase b2))
    | baseEq b1 b2 = return $ Left idS
    | otherwise = mzero
  unifyOne' _ = mzero -- Atom = Cons

equate :: TyDefCtx -> [Type] -> Maybe S
equate tyDefns tys = unify tyDefns eqns
 where
  eqns = zip tys (drop 1 tys)

occurs :: Name Type -> Type -> Bool
occurs x = anyOf fv (== x)

unifyAtoms :: TyDefCtx -> [Atom] -> Maybe (Substitution Atom)
unifyAtoms tyDefns = fmap (fmap fromTyAtom) . equate tyDefns . map TyAtom
 where
  fromTyAtom (TyAtom a) = a
  fromTyAtom _ = error "fromTyAtom on non-TyAtom!"

unifyUAtoms :: TyDefCtx -> [UAtom] -> Maybe (Substitution UAtom)
unifyUAtoms tyDefns = fmap (fmap fromTyAtom) . equate tyDefns . map (TyAtom . uatomToAtom)
 where
  fromTyAtom (TyAtom (ABase b)) = UB b
  fromTyAtom (TyAtom (AVar (U v))) = UV v
  fromTyAtom _ = error "fromTyAtom on wrong thing!"
