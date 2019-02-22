{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Module
-- Copyright   :  (c) 2019 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- The 'ModuleInfo' record representing a disco module, and various
-- functions to manipulate, load, and check modules.
--
-----------------------------------------------------------------------------

module Disco.Module where

import           GHC.Generics                     (Generic)

import           Unbound.Generics.LocallyNameless

import           Control.Lens                     (makeLenses)
import qualified Data.Map                         as M

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Typecheck.Monad            (TyCtx, TyDefCtx)
import           Disco.Types

-- | A definition is a group of clauses, each having a list of
--   patterns that bind names in a term, without the name of the
--   function being defined.  For example, given the concrete syntax
--   @f n (x,y) = n*x + y@, the corresponding 'Defn' would be
--   something like @[n, (x,y)] (n*x + y)@.
data Defn  = Defn (Name ATerm) [Type] Type [Clause]
  deriving (Show, Generic)

-- | A clause in a definition consists of a list of patterns (the LHS
--   of the =) and a term (the RHS).
type Clause = Bind [APattern] ATerm

instance Subst Type Defn

-- | Type checking a module yields a value of type ModuleInfo which contains
--   mapping from terms to their relavent documenation, a mapping from terms to
--   properties, and a mapping from terms to their types.
data ModuleInfo = ModuleInfo
  { _modDocs     :: Ctx Term Docs
  , _modProps    :: Ctx ATerm [AProperty]
  , _modTys      :: TyCtx
  , _modTydefs   :: TyDefCtx
  , _modTermdefs :: Ctx ATerm Defn
  }

makeLenses ''ModuleInfo

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo emptyCtx emptyCtx emptyCtx M.empty emptyCtx
