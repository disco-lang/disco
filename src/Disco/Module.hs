{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Module
-- Copyright   :  (c) 2019 disco team (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The 'ModuleInfo' record representing a disco module, and various
-- functions to manipulate, load, and check modules.
--
-----------------------------------------------------------------------------

module Disco.Module where

import           GHC.Generics                     (Generic)

import           Control.Lens                     (makeLenses)
import           Control.Monad                    (filterM, foldM)
import           Control.Monad.Except             (MonadError, throwError)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Coerce                      (coerce)
import qualified Data.Map                         as M
import           System.Directory                 (doesFileExist)
import           System.FilePath                  (replaceExtension, (</>))

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Eval                       (IErr (..), io)
import           Disco.Typecheck.Monad            (TCError (..), TyCtx,
                                                   TyDefCtx)
import           Disco.Types

import           Paths_disco

------------------------------------------------------------
-- ModuleInfo and related types
------------------------------------------------------------

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

-- | Merges a list of ModuleInfos into one ModuleInfo. Two ModuleInfos are merged by
--   joining their doc, type, type definition, and term contexts. The property context
--   of the new module is the obtained from the second module. If threre are any duplicate
--   type definitions or term definitions, a Typecheck error is thrown.
combineModuleInfo :: (MonadError IErr m) => [ModuleInfo] -> m ModuleInfo
combineModuleInfo mis = foldM combineMods emptyModuleInfo mis
  where combineMods :: (MonadError IErr m) => ModuleInfo -> ModuleInfo -> m ModuleInfo
        combineMods (ModuleInfo d1 _ ty1 tyd1 tm1) (ModuleInfo d2 p2 ty2 tyd2 tm2) =
          case (M.keys $ M.intersection tyd1 tyd2, M.keys $ M.intersection tm1 tm2) of
            ([],[]) -> return $ ModuleInfo (joinCtx d1 d2) p2 (joinCtx ty1 ty2) (M.union tyd1 tyd2) (joinCtx tm1 tm2)
            (x:_, _) -> throwError $ TypeCheckErr $ DuplicateTyDefns (coerce x)
            (_, y:_) -> throwError $ TypeCheckErr $ DuplicateDefns (coerce y)

------------------------------------------------------------
-- Module resolution
------------------------------------------------------------

-- | Given a directory and a module name, relavent directories are searched for the file
--   containing the provided module name. Currently, Disco searches for the module in
--   the standard library directory (lib), and the directory passed in to resolveModule.
resolveModule :: (MonadError IErr m, MonadIO m) => FilePath -> ModName -> m FilePath
resolveModule directory modname = do
  datadir <- io getDataDir
  let fps = map (</> replaceExtension modname "disco") [directory, datadir]
  fexists <- io $ filterM doesFileExist fps
  case fexists of
    []     -> throwError $ ModuleNotFound modname
    (fp:_) -> return fp
