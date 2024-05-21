-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Disco.Pretty.Prec
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Precedence and associativity for pretty-printing.
module Disco.Pretty.Prec where

import Disco.Syntax.Operators

-- Types for storing precedence + associativity together

type Prec = Int

data PA = PA Prec BFixity
  deriving (Show, Eq)   -- Do NOT derive Ord, see note below.

lowerPrec :: PA -> PA -> Bool
lowerPrec (PA p1 a1) (PA p2 a2) = p1 < p2 || (p1 == p2 && a1 /= a2)

-- Note re: lowerPrec: we used to have an unlawful Ord instance defined by
--
--   compare (PA p1 a1) (PA p2 a2) = compare p1 p2 `mappend` (if a1 == a2 then EQ else LT)
--
-- with the idea that we could test whether one precedence was lower
-- than another simply using (<).
--
-- However, this was unlawful since e.g. it does not satisfy x < y ==
-- y > x: If x and y have the same Prec value but different BFixity
-- values, we would have both x < y and y < x.
--
-- In base-4.18 apparently something in the default implementations of
-- Ord methods changed so that e.g. not (PA 2 InR < PA 2 InL).  Hence
-- the 'mparens' method in such cases of nested same-precedence
-- operators was not emitting parentheses in cases where it should.

-- Standard precedence levels

initPA :: PA
initPA = PA 0 InL

ascrPA :: PA
ascrPA = PA 1 InL

funPA :: PA
funPA = PA funPrec InL

rPA :: Int -> PA
rPA n = PA n InR

tarrPA, taddPA, tmulPA, tfunPA :: PA
tarrPA = rPA 1
taddPA = rPA 6
tmulPA = rPA 7
tfunPA = PA 9 InL

-- Converting UOp and BOp

ugetPA :: UOp -> PA
ugetPA op = PA (uPrec op) In

getPA :: BOp -> PA
getPA op = PA (bPrec op) (assoc op)
