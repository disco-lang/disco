{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- TODO: the calls to 'error' should be replaced with logging/error capabilities.

-- |
-- Module      :  Disco.Pretty
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Various pretty-printing facilities for disco.
module Disco.Pretty (
  module Disco.Pretty.DSL,
  module Disco.Pretty,
  module Disco.Pretty.Prec,
  Doc,
)
where

import Prelude hiding ((<>))

import Data.Bifunctor
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Disco.Effects.LFresh
import Polysemy
import Polysemy.Reader
import Unbound.Generics.LocallyNameless (Name)
import Prettyprinter (Doc)
import Disco.Pretty.DSL
import Disco.Pretty.Prec
import Disco.Syntax.Operators

------------------------------------------------------------
-- Utilities for handling precedence and associativity

-- | Convenience function combining 'setPA' and 'mparens', since we
--   often want to simultaneously indicate what the precedence and
--   associativity of a term is, and optionally surround it with
--   parentheses depending on the precedence and associativity of its
--   parent.
withPA :: Member (Reader PA) r => PA -> Sem r (Doc ann) -> Sem r (Doc ann)
withPA pa = mparens pa . setPA pa

-- | Locally set the precedence and associativity within a
--   subcomputation.
setPA :: Member (Reader PA) r => PA -> Sem r a -> Sem r a
setPA = local . const

-- | Mark a subcomputation as pretty-printing a term on the left of an
--   operator (so parentheses can be inserted appropriately, depending
--   on the associativity).
lt :: Member (Reader PA) r => Sem r (Doc ann) -> Sem r (Doc ann)
lt = local (\(PA p _) -> PA p InL)

-- | Mark a subcomputation as pretty-printing a term on the right of
--   an operator (so parentheses can be inserted appropriately,
--   depending on the associativity).
rt :: Member (Reader PA) r => Sem r (Doc ann) -> Sem r (Doc ann)
rt = local (\(PA p _) -> PA p InR)

-- | Optionally surround a pretty-printed term with parentheses,
--   depending on its precedence and associativity (given as the 'PA'
--   argument) and that of its context (given by the ambient 'Reader
--   PA' effect).
mparens :: Member (Reader PA) r => PA -> Sem r (Doc ann) -> Sem r (Doc ann)
mparens pa doc = do
  parentPA <- ask
  (if pa < parentPA then parens else id) doc

------------------------------------------------------------
-- Pretty type class

class Pretty t where
  pretty :: Members '[Reader PA, LFresh] r => t -> Sem r (Doc ann)

prettyStr :: Pretty t => t -> Sem r String
prettyStr = renderDoc . runLFresh . pretty

pretty' :: Pretty t => t -> Sem r (Doc ann)
pretty' = runReader initPA . runLFresh . pretty

------------------------------------------------------------
-- Some standard instances

instance Pretty a => Pretty [a] where
  pretty = brackets . intercalate "," . map pretty

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = do
    let es = map (\(k, v) -> pretty k <+> "->" <+> pretty v) (M.assocs m)
    ds <- setPA initPA $ punctuate "," es
    braces (hsep ds)

instance Pretty a => Pretty (Set a) where
  pretty = braces . intercalate "," . map pretty . S.toList

------------------------------------------------------------
-- Some Disco instances

instance Pretty (Name a) where
  pretty = text . show

instance Pretty TyOp where
  pretty = \case
    Enumerate -> text "enumerate"
    Count -> text "count"

-- | Pretty-print a unary operator, by looking up its concrete syntax
--   in the 'uopMap'.
instance Pretty UOp where
  pretty op = case M.lookup op uopMap of
    Just (OpInfo _ (syn : _) _) ->
      text $ syn ++ (if all isAlpha syn then " " else "")
    _ -> error $ "UOp " ++ show op ++ " not in uopMap!"

-- | Pretty-print a binary operator, by looking up its concrete syntax
--   in the 'bopMap'.
instance Pretty BOp where
  pretty op = case M.lookup op bopMap of
    Just (OpInfo _ (syn : _) _) -> text syn
    _ -> error $ "BOp " ++ show op ++ " not in bopMap!"

--------------------------------------------------
-- Pretty-printing decimals

-- | Pretty-print a rational number using its decimal expansion, in
--   the format @nnn.prefix[rep]...@, with any repeating digits enclosed
--   in square brackets.
prettyDecimal :: Rational -> String
prettyDecimal r = printedDecimal
 where
  (n, d) = properFraction r :: (Integer, Rational)
  (expan, len) = digitalExpansion 10 (numerator d) (denominator d)
  printedDecimal
    | length first102 > 101 || length first102 == 101 && last first102 /= 0 =
        show n ++ "." ++ concatMap show (take 100 expan) ++ "..."
    | rep == [0] =
        show n ++ "." ++ (if null pre then "0" else concatMap show pre)
    | otherwise =
        show n ++ "." ++ concatMap show pre ++ "[" ++ concatMap show rep ++ "]"
   where
    (pre, rep) = splitAt len expan
    first102 = take 102 expan

-- Given a list, find the indices of the list giving the first and
-- second occurrence of the first element to repeat, or Nothing if
-- there are no repeats.
findRep :: Ord a => [a] -> ([a], Int)
findRep = findRep' M.empty 0

findRep' :: Ord a => M.Map a Int -> Int -> [a] -> ([a], Int)
findRep' _ _ [] = error "Impossible. Empty list in findRep'"
findRep' prevs ix (x : xs)
  | x `M.member` prevs = ([], prevs M.! x)
  | otherwise = first (x :) $ findRep' (M.insert x ix prevs) (ix + 1) xs

-- | @digitalExpansion b n d@ takes the numerator and denominator of a
--   fraction n/d between 0 and 1, and returns a pair of (1) a list of
--   digits @ds@, and (2) a nonnegative integer k such that @splitAt k
--   ds = (prefix, rep)@, where the infinite base-b expansion of
--   n/d is 0.@(prefix ++ cycle rep)@.  For example,
--
--   > digitalExpansion 10 1 4  = ([2,5,0], 2)
--   > digitalExpansion 10 1 7  = ([1,4,2,8,5,7], 0)
--   > digitalExpansion 10 3 28 = ([1,0,7,1,4,2,8,5], 2)
--   > digitalExpansion 2  1 5  = ([0,0,1,1], 0)
--
--   It works by performing the standard long division algorithm, and
--   looking for the first time that the remainder repeats.
digitalExpansion :: Integer -> Integer -> Integer -> ([Integer], Int)
digitalExpansion b n d = digits
 where
  longDivStep (_, r) = (b * r) `divMod` d
  res = tail $ iterate longDivStep (0, n)
  digits = first (map fst) (findRep res)
