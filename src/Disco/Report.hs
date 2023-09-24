-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- The benefit of having our own deeply-embedded type for pretty
-- printing things would be so we can render it in different backend
-- formats later (text, LaTeX, HTML, ...) so at some point it may be
-- worth doing it.  The idea would be to mostly replicate the
-- interface of the pretty-printing library currently being used, so
-- that a lot of code could just be kept unchanged.

-- |
-- Module      :  Disco.Report
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
module Disco.Report where

import Data.List (intersperse)

data Report
  = RTxt String
  | RSeq [Report]
  | RVSeq [Report]
  | RList [Report]
  | RNest Report
  deriving (Show)

text :: String -> Report
text = RTxt

hcat :: [Report] -> Report
hcat = RSeq

hsep :: [Report] -> Report
hsep = hcat . intersperse (text " ")

vcat :: [Report] -> Report
vcat = RVSeq

vsep :: [Report] -> Report
vsep = vcat . intersperse (text "")

list :: [Report] -> Report
list = RList

nest :: Report -> Report
nest = RNest

------------------------------------------------------------
