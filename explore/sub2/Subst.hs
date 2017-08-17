module Subst where

import           Control.Arrow (second)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as F

import           Unbound.Generics.LocallyNameless

type S' a = [(Name a, a)]

idS :: S' a
idS = []

(|->) :: Name a -> a -> S' a
x |-> t = [(x,t)]

(@@) :: Subst a a => S' a -> S' a -> S' a
s1 @@ s2 = (map . second) (substs s1) s2 ++ s1

compose :: (Subst a a, Foldable t) => t (S' a) -> S' a
compose = F.foldr (@@) idS
