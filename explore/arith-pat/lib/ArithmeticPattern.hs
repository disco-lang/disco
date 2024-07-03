module ArithmeticPattern where

import Prelude hiding (elem, null, subtract)
import SemilinearSet

type Pattern = SS

-- Pattern Constructions -------------------------------------------------------

-- | Constructs a pattern with a natrual number variable.
mkNatPattern :: Int -> Int -> Pattern
mkNatPattern x p = SS [LS x p]

-- | Constructs a pattern with an integer variable.
mkIntPattern :: Int -> Int -> Pattern
mkIntPattern x p = SS [LS x p, LS x (-p)]

-- | Constructs a pattern with no variable, representing a single number.
mkConstPattern :: Int -> Pattern
mkConstPattern x = SS [LS x 0]

-- | The pattern covering nothing.
empty :: Pattern
empty = SS []

-- | The pattern covering all natural numbers; i.e. the nonnegative integers.
nats :: Pattern
nats = mkNatPattern 0 1

-- | The pattern covering all integers.
ints :: Pattern
ints = mkIntPattern 0 1

-- Pattern Operations ----------------------------------------------------------

-- | Lists all numbers covered by this pattern, in ascending order by absolute
-- value.
toList :: Pattern -> [Int]
toList = toListSS

-- | Checks if a pattern covers nothing.
null :: Pattern -> Bool
null (SS []) = True
null       _ = False

-- | Checks if a number is covered by a pattern.
elem :: Int -> Pattern -> Bool
elem e (SS lss) = any (not . null . intersectWithSingle e) lss

-- | Determines equality of patterns by checking if they are both a subset of
-- the other.
equal :: Pattern -> Pattern -> Bool
equal a b = subset a b && subset b a

-- | Unions two patterns.
union :: Pattern -> Pattern -> Pattern
union = unionSS

-- | Intersects two patterns.
intersect :: Pattern -> Pattern -> Pattern
intersect = intersectSS

-- | Complements a pattern, inverting its coverage.
complement :: Pattern -> Pattern
complement = complementSS

-- | Checks if the first pattern is entirely covered by the second.
subset :: Pattern -> Pattern -> Bool
subset a b = null $ subtract a b

-- | Checks if the first pattern entirely covers the second.
superset :: Pattern -> Pattern -> Bool
superset = flip subset

-- | Subtracts the second pattern from the first.
subtract :: Pattern -> Pattern -> Pattern
subtract a b = intersect a $ complement b
