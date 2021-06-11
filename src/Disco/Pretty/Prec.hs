-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Pretty.Prec
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Precedence and associativity for pretty-printing.
--
-----------------------------------------------------------------------------

module Disco.Pretty.Prec where

import           Disco.Syntax.Operators

-- Types for storing precedence + associativity together

type Prec = Int

data PA = PA Prec BFixity
  deriving (Show, Eq)

instance Ord PA where
  compare (PA p1 a1) (PA p2 a2) = compare p1 p2 `mappend` (if a1 == a2 then EQ else LT)

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

