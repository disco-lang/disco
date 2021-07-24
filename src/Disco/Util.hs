-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Util
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Miscellaneous utilities.
--
-----------------------------------------------------------------------------

module Disco.Util where

infixr 1 ==>

-- | A synonym for pairing which makes convenient syntax for
--   constructing literal maps via M.fromList.
(==>) :: a -> b -> (a,b)
(==>) = (,)
