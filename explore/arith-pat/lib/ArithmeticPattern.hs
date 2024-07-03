module ArithmeticPattern where

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

emptyPattern :: Pattern
emptyPattern = SS []

allNatsPattern :: Pattern
allNatsPattern = mkNatPattern 0 1

allIntsPattern :: Pattern
allIntsPattern = mkIntPattern 0 1

-- Coverage Checking -----------------------------------------------------------

-- | Checks if a number is covered by a pattern.
elemPattern :: Int -> Pattern -> Bool
elemPattern e (SS lss) = any (notNullSS . intersectWithSingle e) lss
  where notNullSS (SS x) = not . null $ x

-- | Subtracts 
subtractPattern :: Pattern -> Pattern -> Pattern
subtractPattern a b = intersectSS a (complementSS b)

-- | Generates a (potentially infinite) list of natural numbers not covered by
-- any of the given patterns. If the list is empty, then the pattern coverage is
-- complete.
missingNats :: [Pattern] -> [Int]
missingNats pats = toListSS $ intersectSS allNatsPattern $ complementSS unionSet
  where unionSet = foldr unionSS (SS []) pats

-- | Generates a (potentially infinite) list of integers not covered by any of
-- the given patterns. If the list is empty, then the pattern coverage is
-- complete.
missingInts :: [Pattern] -> [Int]
missingInts pats = toListSS $ complementSS unionSet
  where unionSet = foldr unionSS emptyPattern pats

-- | Checks whether a set of patterns covers the natural numbers. Shorthand for
-- `null . missingNats`.
coversNats :: [Pattern] -> Bool
coversNats = null . missingNats

-- | Checks whether a set of patterns covers the integers. Shorthand for
-- `null . missingInts`.
coversInts :: [Pattern] -> Bool
coversInts = null . missingInts
