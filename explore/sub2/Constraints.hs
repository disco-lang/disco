{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Constraints where

import GHC.Generics (Generic)

import Unbound.Generics.LocallyNameless

import Types

-- | Equality constraint between two types.
data Eqn t where
  (:=:) :: t -> t -> Eqn t
  deriving (Eq, Show, Generic)

-- | Subtyping constraint between two types.
data Ineqn t where
  (:<:) :: t -> t -> Ineqn t
  deriving (Eq, Show, Generic)

instance Alpha t   => Alpha (Eqn t)
instance Alpha t   => Alpha (Ineqn t)

instance Subst t t => Subst t (Eqn t)
instance Subst t t => Subst t (Ineqn t)

toEqn :: Ineqn t -> Eqn t
toEqn (t1 :<: t2) = t1 :=: t2

-- | A constraint is either an equality or a subtyping constraint.
type Constraint t = Either (Eqn t) (Ineqn t)

-- | Construct an equality constraint.
(===) :: t -> t -> Constraint t
t1 === t2 = Left (t1 :=: t2)

-- | Construct an inequality (subtyping) constraint constraint.
(=<=) :: t -> t -> Constraint t
t1 =<= t2 = Right (t1 :<: t2)

