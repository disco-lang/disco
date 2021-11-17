{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Data
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Some orphan 'Data' instances.
--
-----------------------------------------------------------------------------

module Disco.Data where

import           Unbound.Generics.LocallyNameless.Bind
import           Unbound.Generics.LocallyNameless.Embed
import           Unbound.Generics.LocallyNameless.Name

import           Data.Data                               (Data)
import           Unbound.Generics.LocallyNameless.Rebind

------------------------------------------------------------
-- Some orphan instances
------------------------------------------------------------

deriving instance (Data a, Data b) => Data (Bind a b)
deriving instance Data t => Data (Embed t)
deriving instance (Data a, Data b) => Data (Rebind a b)
deriving instance Data a => Data (Name a)

