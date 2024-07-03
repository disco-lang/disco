module SemilinearSet where

-- Types -----------------------------------------------------------------------

data LS = LS Int Int deriving (Show, Eq)  -- (initial, period)

newtype SS = SS [LS] deriving (Show, Eq)     -- union

-- List Representation ---------------------------------------------------------

-- | Represents a linear set as a list of numbers.
toListLS :: LS -> [Int]
toListLS (LS x p) = if p == 0 then [x] else [x + p * i | i <- [0..]]

-- | Represents a semilinear set as a list of numbers, sorted by absolute value
-- in ascending order.
toListSS :: SS -> [Int]
toListSS (SS lss) = answer
  where
    downLSs = filter (\(LS _ p) -> p < 0) lss
    upLSs = filter (\(LS _ p) -> p >= 0) lss
    downLists = map toListLS downLSs
    upLists = map toListLS upLSs
    downList = foldr (mergeSortedLists (>)) [] downLists
    upList = foldr (mergeSortedLists (<)) [] upLists
    (downPositives, downRest) = span (>= 0) downList
    (upNegatives, upRest) = span (< 0) upList
    allNegatives = mergeSortedLists (>) (reverse upNegatives) downRest
    allPositives = mergeSortedLists (<) (reverse downPositives) upRest
    answer = mergeSortedLists (\x y -> abs x < abs y) allNegatives allPositives

-- | Merges two lists together via a selection function. The selection function
-- returning `True` means that the head of the first list will be taken first.
-- If the heads are equal, the first is taken and the second is discarded.
mergeSortedLists :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeSortedLists cmp (x:xs) (y:ys)
  | x == y    = x : mergeSortedLists cmp xs ys
  | cmp x y   = x : mergeSortedLists cmp xs (y:ys)
  | otherwise = y : mergeSortedLists cmp (x:xs) ys
mergeSortedLists _ x y = x ++ y

-- Set Helpers -----------------------------------------------------------------

-- | Negates the initial and period of a linear set, effectively mapping
-- negation over the elements of the set.
flipDirLS :: LS -> LS
flipDirLS (LS x p) = LS (-x) (-p)

-- | Maps `flipDirLS` over the linear sets within a semilinear set.
flipDirSS :: SS -> SS
flipDirSS (SS lss) = SS $ map flipDirLS lss

-- | Represents a linear set as an equivalent semilinear set.
toSS :: LS -> SS
toSS ls = SS [ls]

-- | Checks if a number is an element of a linear set.
containsLS :: Int -> LS -> Bool
containsLS n (LS x p) = case compare p 0 of
  LT -> n <= x && modEquiv p n x
  GT -> n >= x && modEquiv p n x
  EQ -> n == x

-- | Sorts two linear sets by their initial value in descending order.
sort2LS :: LS -> LS -> ((Int, Int), (Int, Int))
sort2LS (LS x1 p1) (LS x2 p2) = if x1 > x2
  then ((x1, p1), (x2, p2))
  else ((x2, p2), (x1, p1))

-- Set Operations --------------------------------------------------------------

-- | Intersects two singleton linear sets.
intersectTwoSingles :: Int -> Int -> SS
intersectTwoSingles x y = SS [LS x 0 | x == y]

-- | Intersects a singleton linear set with a non-singleton one.
intersectWithSingle :: Int -> LS -> SS
intersectWithSingle s ls = SS [LS s 0 | containsLS s ls]

-- | Intersects two linear sets when both have positive periods. Expects
-- arguments to have been sorted via `sort2LS`.
intersectSameDir :: LS -> LS -> SS
intersectSameDir lsa lsb = answer
  where
    ((x1, p1), (x2, p2)) = sort2LS lsa lsb
    diff = x2 - x1
    p2dg = p2 `div` g
    gp = gcd p1 p2
    g = gcd gp $ diff `mod` p2
    i = modInv p2dg (p1 `div` g)
    k = mod (i * diff `div` g) p2dg
    answer = if g == gp
      then SS [LS (p1 * k + x1) (lcm p1 p2)]
      else SS []

-- | Intersects two linear sets whose periods are opposite in sign. Expects the
-- first linear set to have a negative period, and the second to be positive.
intersectOppDir :: LS -> LS -> SS
intersectOppDir (LS xd pd) (LS xu pu) = answer
  where
    answer = SS $ map (\n -> LS n 0) filtered
    filtered = filter (\n -> modEquiv pd n xd && modEquiv pu n xu) [xu..xd]

-- | Intersects any two linear sets; the result is semilinear.
intersectLS :: LS -> LS -> SS
intersectLS lsa@(LS x1 p1) lsb@(LS x2 p2) = case (compare p1 0, compare p2 0) of
  (EQ, EQ) -> intersectTwoSingles x1 x2
  (EQ,  _) -> intersectWithSingle x1 lsb
  ( _, EQ) -> intersectWithSingle x2 lsa
  (LT, GT) -> intersectOppDir lsa lsb
  (GT, LT) -> intersectOppDir lsb lsa
  (GT, GT) -> intersectSameDir lsa lsb
  (LT, LT) -> flipDirSS $ intersectSameDir (flipDirLS lsa) (flipDirLS lsb)

-- | Intersects two semilinear sets. This is done by pairwise intersecting the
-- component linear sets and unioning those intersections.
intersectSS :: SS -> SS -> SS
intersectSS (SS as) (SS bs) = case intersectLS <$> as <*> bs of
  []     -> SS []
  (c:cs) -> foldr unionSS c cs

-- | Unions two semilinear sets; a trivial operation due to their structure.
unionSS :: SS -> SS -> SS
unionSS (SS a) (SS b) = SS $ a ++ b

-- | Returns the set complement of the given linear set, which is semilinear.
complementLS :: LS -> SS
complementLS (LS x p) = case compare p 0 of
  GT -> SS $ LS (x - 1) (-1) : [LS (x + i) p | i <- [1 .. p - 1]]
  LT -> SS $ LS (x + 1)   1  : [LS (x + i) p | i <- [p + 1 .. -1]]
  EQ -> SS  [LS (x - 1) (-1), LS (x + 1) 1]

-- | Returns the set complement of the given semilinear set. This is done by
-- complementing the component linear sets and intersecting them.
complementSS :: SS -> SS
complementSS (SS lss) = case map complementLS lss of
  [] -> SS [LS 0 1, LS (-1) (-1)]
  (x:xs) -> foldr intersectSS x xs

-- | Returns the set difference of two linear sets; i.e. the intersection of A
-- with the complement of B.
subtractSS :: SS -> SS -> SS
subtractSS ssa ssb = intersectSS ssa (complementSS ssb)

-- Math Helpers ----------------------------------------------------------------

-- | Runs the Extended Euclidean Algorithm. `egcd a b` returns `(gcd a b, x, y)`
-- such that `a*x + b*y == gcd a b`.
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b = (g, t - d * s, s)
  where
    (g, s, t) = egcd m a
    (d, m) = divMod b a

-- | Calculates the multiplicative inverse for a given modulus.
-- `modInv m a = x` such that `mod (a*x) m == gcd a m`.
modInv :: Int -> Int -> Int
modInv m a = mod x m
  where (_, x, _) = egcd a m

-- | Determines whether two numbers are equivalent under a modulus.
modEquiv :: Int -> Int -> Int -> Bool
modEquiv m a b = mod a m == mod b m
