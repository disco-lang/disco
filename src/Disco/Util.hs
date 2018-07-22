-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Util
-- Copyright   :  (c) 2018 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
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
