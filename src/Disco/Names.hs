{-# LANGUAGE DeriveAnyClass #-}

module Disco.Names
  ( -- * Module names and provenance
    ModuleProvenance(..), ModuleName(..)
    -- * Basic variable names
  , Name, string2Name, name2String
    -- * Qualified variable names
  , NameProvenance(..), QName(..), localName, (.-)
  ) where

import           Data.Typeable (Typeable)

------------------------------------------------------------
-- Module names
------------------------------------------------------------

-- | Where did a module come from?
data ModuleProvenance
  = Dir FilePath -- ^ From a particular directory (relative to cwd)
  | Stdlib       -- ^ From the standard library
  deriving (Eq, Ord, Show)

-- | The name of a module.
data ModuleName
  = REPLModule   -- ^ The special top-level "module" consisting of
                 -- what has been entered at the REPL.
  | Named ModuleProvenance String
                 -- ^ A named module, with its name and provenance.
  deriving (Eq, Ord, Show)

------------------------------------------------------------
-- Variable names
------------------------------------------------------------

-- | XXX
data Name a = Name { nameStr :: String, nameSuffix :: Int }
  deriving (Eq, Ord, Show)

data AnyName where
  AnyName :: Typeable a => Name a -> AnyName
  deriving (Eq, Ord, Show)

-- | Make a @Name a@ from a string.
string2Name :: String -> Name a
string2Name x = Name x 0

-- | Get the string part of a name.
name2String :: Name a -> String
name2String = nameStr

------------------------------------------------------------
-- Qualified names
------------------------------------------------------------

-- | Where did a name come from?
data NameProvenance
  = LocalName                    -- ^ The name is locally bound
  | QualifiedName ModuleName     -- ^ The name is exported by the given module
  deriving (Eq, Ord, Show)

-- | A @QName@, or qualified name, is a 'Name' paired with its
--   'NameProvenance'.
data QName a = QName { qnameProvenance :: NameProvenance, qname :: Name a }
  deriving (Eq, Ord, Show)

-- | Create a locally bound qualified name.
localName :: Name a -> QName a
localName = QName LocalName

-- | Create a module-bound qualified name.
(.-) :: ModuleName -> Name a -> QName a
m .- x = QName (QualifiedName m) x

