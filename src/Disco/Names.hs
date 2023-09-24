{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      :  Disco.Names
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Names for modules and identifiers.
module Disco.Names (
  -- * Modules and their provenance
  ModuleProvenance (..),
  ModuleName (..),

  -- * Names and their provenance
  NameProvenance (..),
  QName (..),
  isFree,
  localName,
  (.-),

  -- * Name-related utilities
  fvQ,
  substQ,
  substsQ,
) where

import Control.Lens (Traversal', filtered)
import Data.Data (Data)
import Data.Data.Lens (template)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.FilePath (dropExtension)
import Unbound.Generics.LocallyNameless
import Prelude hiding ((<>))

import Disco.Pretty
import Disco.Types

------------------------------------------------------------
-- Modules
------------------------------------------------------------

-- | Where did a module come from?
data ModuleProvenance
  = -- | From a particular directory (relative to cwd)
    Dir FilePath
  | -- | From the standard library
    Stdlib
  deriving (Eq, Ord, Show, Generic, Data, Alpha, Subst Type)

-- | The name of a module.
data ModuleName
  = -- | The special top-level "module" consisting of
    -- what has been entered at the REPL.
    REPLModule
  | -- | A named module, with its name and provenance.
    Named ModuleProvenance String
  deriving (Eq, Ord, Show, Generic, Data, Alpha, Subst Type)

------------------------------------------------------------
-- Names
------------------------------------------------------------

-- | Where did a name come from?
data NameProvenance
  = -- | The name is locally bound
    LocalName
  | -- | The name is exported by the given module
    QualifiedName ModuleName
  deriving (Eq, Ord, Show, Generic, Data, Alpha, Subst Type)

-- | A @QName@, or qualified name, is a 'Name' paired with its
--   'NameProvenance'.
data QName a = QName {qnameProvenance :: NameProvenance, qname :: Name a}
  deriving (Eq, Ord, Show, Generic, Data, Alpha, Subst Type)

-- | Does this name correspond to a free variable?
isFree :: QName a -> Bool
isFree (QName (QualifiedName _) _) = True
isFree (QName LocalName n) = isFreeName n

-- | Create a locally bound qualified name.
localName :: Name a -> QName a
localName = QName LocalName

-- | Create a module-bound qualified name.
(.-) :: ModuleName -> Name a -> QName a
m .- x = QName (QualifiedName m) x

------------------------------------------------------------
-- Free variables and substitution
------------------------------------------------------------

-- | The @unbound-generics@ library gives us free variables for free.
--   But when dealing with typed and desugared ASTs, we want all the
--   free 'QName's instead of just 'Name's.
fvQ :: (Data t, Typeable e) => Traversal' t (QName e)
fvQ = template . filtered isFree

substQ :: Subst b a => QName b -> b -> a -> a
substQ = undefined

substsQ :: Subst b a => [(QName b, b)] -> a -> a
substsQ = undefined

------------------------------------------------------------
-- Pretty-printing
------------------------------------------------------------

instance Pretty ModuleName where
  pretty REPLModule = "REPL"
  pretty (Named (Dir _) s) = text (dropExtension s)
  pretty (Named Stdlib s) = text (dropExtension s)

instance Pretty (QName a) where
  pretty (QName LocalName x) = pretty x
  pretty (QName (QualifiedName mn) x) = pretty mn <> "." <> pretty x
