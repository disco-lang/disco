{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module MatchTree where

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Types as Ty

data MatchTree where
  Pair :: MatchTree -> MatchTree -> MatchTree
  Either :: [MatchTree] -> [MatchTree] -> MatchTree
  Status :: Status -> MatchTree
  deriving (Show, Eq)

-- Not should be read as "everything except for"
data Status where
  Is :: Maybe Ty.DataConstructor -> Status
  Not :: [Ty.DataConstructor] -> Status
  deriving (Show, Eq)

full = Status (Not [])

empty = Status (Is Nothing)

intCon i = Ty.DataConstructor {Ty.dcName = T.pack $ show i, Ty.dcTypes = []}

isInt i = Status (Is (Just (intCon i)))

c1 = Pair (isInt 1) full

c2 = Pair full (isInt 2)

final = ([full] \\\ c1) \\\ c2

final2 = ([full] \\\ c2) \\\ c1

true = Status $ Is $ Just $ head $ Ty.dataCons Ty.bool

e1 = Pair (Either [(isInt 10)] []) (isInt 2)

e2 = Pair (Either [(isInt 4)] []) (isInt 5)

e3 = Pair (Either [] [true]) (isInt 5)

a1 = Either [true] []

b1 = Pair (isInt 7) (isInt 5)

(\\) :: MatchTree -> MatchTree -> [MatchTree]
(\\) = treeMinus

(\\\) :: [MatchTree] -> MatchTree -> [MatchTree]
a \\\ b = concatMap (\\ b) a

treeMinus :: MatchTree -> MatchTree -> [MatchTree]
treeMinus t1 t2 = case (t1, t2) of
  (Status (Is Nothing), _) -> []
  (t, Status (Is Nothing)) -> [t]
  (_, Status (Not [])) -> []
  (Status s1, Status s2) -> case statMinus s1 s2 of
    Is Nothing -> []
    s -> [Status s]
  (Status (Not []), p@(Pair _ _)) -> treeMinus (Pair (Status (Not [])) (Status (Not []))) p
  (Status (Not []), e@(Either _ _)) -> treeMinus (Either [Status (Not [])] [Status (Not [])]) e
  (Status _, Pair _ _) -> error "type error"
  (Pair _ _, Status _) -> error "type error2"
  (Status _, Either _ _) -> error "type error3"
  (Either _ _, Status _) -> error "type error4"
  (Either _ _, Pair _ _) -> error "type error5"
  (Pair _ _, Either _ _) -> error "type error6"
  (Pair a b, Pair c d) ->
    map mkPairL (a \\ c) ++ map mkPairR (b \\ d) ++ both
    where
      mkPairL aSubC = Pair aSubC d
      mkPairR bSubD = Pair c bSubD
      both = [Pair aSubC bSubD | aSubC <- a \\ c, bSubD <- b \\ d]
      c' = treeIntersect a c
      d' = treeIntersect b d
      -- aSubC = a \\ c'
  (Either a b, Either c d) ->
    [Either left right]
    where
      -- l = foldr a (flip map (\\)) c
      left = concat [x \\ y | x <- a, y <- c]
      right = concat [x \\ y | x <- b, y <- d]

treeIntersect :: MatchTree -> MatchTree -> [MatchTree]
treeIntersect m1 m2 = case (m1, m2) of
  (Status (Is Nothing), _) -> []
  (_, Status (Is Nothing)) -> []
  (a, Status (Not [])) -> [a]
  (Status (Not []), a) -> [a]
  (Status s1, Status s2) -> case statIntersect s1 s2 of
    Is Nothing -> []
    s -> [Status s]
  (Status _, Pair _ _) -> error "type error"
  (Pair _ _, Status _) -> error "type error2"
  (Status _, Either _ _) -> error "type error3"
  (Either _ _, Status _) -> error "type error4"
  (Either _ _, Pair _ _) -> error "type error5"
  (Pair _ _, Either _ _) -> error "type error6"
  (Pair a b, Pair c d) -> error "pairs"
    -- map mkPairL (a \\ c) ++ map mkPairR (b \\ d) ++ both
    -- where
    --   mkPairL aSubC = Pair aSubC d
    --   mkPairR bSubD = Pair c bSubD
    --   both = [Pair aSubC bSubD | aSubC <- a \\ c, bSubD <- b \\ d]
      -- c' = statIntersect a c
      -- d' = statIntersect b d
  (Either a b, Either c d) -> error "eithers"
    -- [Either left right]
    -- where
    --   -- l = foldr a (flip map (\\)) c
    --   left = concat [x \\ y | x <- a, y <- c]
    --   right = concat [x \\ y | x <- b, y <- d]
  

-- (a-c)*(b-d) + c*(b-d) + (a-c)*d

-- (a X b) \ (c X d) = (a X (b\d)) U ((a\c) X b)
-- I eventually stumbled onto the correct solution,
-- here a proof of it:
-- https://proofwiki.org/wiki/Set_Difference_of_Cartesian_Products
-- This actually doesn't do quite what we want,
-- because it doesn't return a disjoint union, the union overlaps
--
-- see b1 and foo6 for example
-- this may be the fatal flaw that makes this not work
-- the above formula seems to be required for associativity
-- possibly required for correctness at all
--
-- also either seems to not be working, but
-- that may be an artifact of the above issue

statMinus :: Status -> Status -> Status
statMinus s1 s2 = case (s1, s2) of
  (s, Is Nothing) -> s
  (_, Not []) -> Is Nothing
  (_, Not _) -> error "Attempted to subtract negative information"
  (Is Nothing, _) -> Is Nothing
  (Not [], Is (Just x)) -> Not [x]
  (Not xs, Is (Just x)) -> Not (x : xs)
  (Is (Just x), Is (Just x')) -> if x == x' then Is Nothing else Is (Just x)

statIntersect :: Status -> Status -> Status
statIntersect s1 s2 = case (s1, s2) of
  (Is Nothing, _) -> Is Nothing
  (_, Is Nothing) -> Is Nothing
  (Not a, Not b) -> Not (a ++ b)
  (Is (Just k), Not b) -> if k `elem` b then Is Nothing else Is (Just k)
  (Not b, Is (Just k)) -> if k `elem` b then Is Nothing else Is (Just k)
  (Is (Just k1), Is (Just k2)) -> if k1 == k2 then Is (Just k1) else Is Nothing

-- (IsInt i1, IsInt i2) -> if i1 == i2 then Empty else IsInt i1

-- (Full, NotInt is) -> error "what2?"
-- (NotInt is, NotInt is2) -> error "what?"
-- (IsInt i, NotInt is) -> error "what?"
