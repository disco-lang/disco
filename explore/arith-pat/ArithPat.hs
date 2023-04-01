import Prelude hiding (elem)
import Prelude qualified as P

import Control.Applicative

-- Key insight: of course we could simply use (Integer -> Bool) to
-- represent sets of integers; then interpreting patterns, taking
-- unions, etc. would be very easy.  But testing for equality of such
-- sets (e.g. testing for complete coverage by testing equality with Z
-- itself) would be undecidable!  So the name of the game is to add a
-- bit more first-order information to represent the structure of the
-- specific kinds of sets we can get, so that we can decide equality.

data ZSet = ZSet Integer (Integer -> Bool)
  -- modulus m paired with indicator function on [0..m)
  -- ZSet m B represents the set { am + b | a ∈ ℤ, b ∈ B }

ints :: ZSet
ints = ZSet 1 (const True)

odds :: ZSet
odds = ZSet 2 (==1)

evens :: ZSet
evens = ZSet 2 (==0)

m31 :: ZSet
m31 = ZSet 3 (==1)

m635 :: ZSet
m635 = ZSet 6 (`P.elem` [3,5])

elem :: ZSet -> Integer -> Bool
elem (ZSet m b) = b . (`mod` m)

expand :: Integer -> ZSet -> ZSet
expand k (ZSet m b) = ZSet (k*m) (\x -> b (x `mod` m))

union :: ZSet -> ZSet -> ZSet
union s1@(ZSet m1 _) s2@(ZSet m2 _) = ZSet (lcm m1 m2) (liftA2 (||) (elem s1) (elem s2))

instance Eq ZSet where
  s1@(ZSet m1 _) == s2@(ZSet m2 _) = all (liftA2 (==) (elem s1) (elem s2)) [0 .. m-1]
    where
      m = lcm m1 m2

