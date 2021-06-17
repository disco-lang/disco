{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Capability
-- Copyright   :  (c) 2021 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Utilities related to the 'capability' library.
--
-----------------------------------------------------------------------------

module Disco.Capability where

import qualified Capability.Constraints as CC
import           Capability.Error
import           Capability.Reader
import           Capability.Writer

type Rd tag = HasReader' tag
type Wr tag = HasWriter' tag
type Th tag = HasThrow' tag
type Ct tag = HasCatch' tag

type Has cs m = CC.All cs m
