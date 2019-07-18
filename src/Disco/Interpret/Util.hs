-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interpret.Util
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Disco.Interpret.Util where

import           Disco.Eval

-- | A convenience function for creating a default @VNum@ value with a
--   default (@Fractional@) flag.
vnum :: Rational -> Value
vnum = VNum mempty

-- | Turn any instance of @Enum@ into a @Value@, by creating a
--   constructor with an index corresponding to the enum value.
mkEnum :: Enum e => e -> Value
mkEnum e = VCons (fromEnum e) []
