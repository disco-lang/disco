{-# LANGUAGE DeriveAnyClass #-}

module Disco.Names
  ( -- * Names and provenance
    ModuleProvenance(..), ModuleName(..)
  , NameProvenance(..), QName(..), localName, (.-)
  ) where

import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless

import           Disco.Types

-- | Where did a module come from?
data ModuleProvenance
  = Dir FilePath -- ^ From a particular directory (relative to cwd)
  | Stdlib       -- ^ From the standard library
  deriving (Eq, Ord, Show, Generic, Alpha, Subst Type)

-- | The name of a module.
data ModuleName
  = REPLModule   -- ^ The special top-level "module" consisting of
                 -- what has been entered at the REPL.
  | Named ModuleProvenance String
                 -- ^ A named module, with its name and provenance.
  deriving (Eq, Ord, Show, Generic, Alpha, Subst Type)

-- | Where did a name come from?
data NameProvenance
  = LocalName                    -- ^ The name is locally bound
  | QualifiedName ModuleName     -- ^ The name is exported by the given module
  deriving (Eq, Ord, Show, Generic, Alpha, Subst Type)

-- | A @QName@, or qualified name, is a 'Name' paired with its
--   'NameProvenance'.
data QName a = QName { qnameProvenance :: NameProvenance, qname :: Name a }
  deriving (Eq, Ord, Show, Generic, Alpha, Subst Type)

-- | Create a locally bound qualified name.
localName :: Name a -> QName a
localName = QName LocalName

-- | Create a module-bound qualified name.
(.-) :: ModuleName -> Name a -> QName a
m .- x = QName (QualifiedName m) x

