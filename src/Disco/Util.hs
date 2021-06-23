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

import           Control.Monad.IO.Class

infixr 1 ==>

-- | A synonym for pairing which makes convenient syntax for
--   constructing literal maps via M.fromList.
(==>) :: a -> b -> (a,b)
(==>) = (,)

io :: MonadIO m => IO a -> m a
io = liftIO

iputStrLn :: MonadIO m => String -> m ()
iputStrLn = io . putStrLn

iputStr :: MonadIO m => String -> m ()
iputStr = io . putStr

iprint :: (MonadIO m, Show a) => a -> m ()
iprint = io . print
