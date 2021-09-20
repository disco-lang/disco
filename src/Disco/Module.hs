{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Module
-- Copyright   :  (c) 2019 disco team (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The 'ModuleInfo' record representing a disco module, and functions
-- to resolve the location of a module on disk.
--
-----------------------------------------------------------------------------

module Disco.Module where

import           GHC.Generics                     (Generic)

import           Control.Lens                     (makeLenses)
import           Control.Monad                    (filterM, foldM)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Coerce                      (coerce)
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Maybe                       (listToMaybe)
import qualified Data.Set                         as S
import           System.Directory                 (doesFileExist)
import           System.FilePath                  (replaceExtension, (</>))

import           Unbound.Generics.LocallyNameless (Alpha, Bind, Name, Subst)

import           Polysemy
import           Polysemy.Error

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Extensions
import           Disco.Typecheck.Monad            (TCError (..), TyCtx)
import           Disco.Types

import           Paths_disco

------------------------------------------------------------
-- ModuleInfo and related types
------------------------------------------------------------

-- | When loading a module, we could be loading it from code entered
-- at the REPL, or from a standalone file.  The two modes have
-- slightly different behavior.
data LoadingMode = REPL | Standalone

-- | A definition consists of a name being defined, the types of any
--   pattern arguments (each clause must have the same number of
--   patterns), the type of the body of each clause, and a list of
--   clauses.  For example,
--
--   @
--   f x (0,z) = 3*x + z > 5
--   f x (y,z) = z == 9
--   @
--
--   might look like @Defn f [Z, Z*Z] B [clause 1 ..., clause 2 ...]@
data Defn  = Defn (Name ATerm) [Type] Type [Clause]
  deriving (Show, Generic, Alpha)

-- | A clause in a definition consists of a list of patterns (the LHS
--   of the =) and a term (the RHS).  For example, given the concrete
--   syntax @f n (x,y) = n*x + y@, the corresponding 'Clause' would be
--   something like @[n, (x,y)] (n*x + y)@.
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
  , _modTerms    :: [(ATerm, PolyType)]
  , _modExts     :: ExtSet
  }

makeLenses ''ModuleInfo

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo emptyCtx emptyCtx emptyCtx M.empty emptyCtx [] S.empty

-- | Merges a list of ModuleInfos into one ModuleInfo. Two ModuleInfos
--   are merged by joining their doc, type, type definition, and term
--   contexts. The property context of the new module is the obtained
--   from the second module. If threre are any duplicate type
--   definitions or term definitions in Standalone mode, a Typecheck
--   error is thrown; in REPL mode, definitions from later modules
--   override earlier ones.
combineModuleInfo :: Member (Error TCError) r => LoadingMode -> [ModuleInfo] -> Sem r ModuleInfo
combineModuleInfo mode = foldM combineMods emptyModuleInfo
  where
    combineMods :: Member (Error TCError) r => ModuleInfo -> ModuleInfo -> Sem r ModuleInfo
    combineMods
      (ModuleInfo d1 _ ty1 tyd1 tm1 tms1 es1)
      (ModuleInfo d2 p2 ty2 tyd2 tm2 tms2 es2) =
      case (findDups tyd1 tyd2, findDups tm1 tm2) of
        (Nothing, Nothing) -> return $
          ModuleInfo
            (joinCtx d2 d1)
            p2
            (joinCtx ty2 ty1)
            (M.union tyd2 tyd1)
            (joinCtx tm2 tm1)
            (tms1 ++ tms2)
            (es1 `S.union` es2)
        (Just x, _) -> throw $ DuplicateTyDefns (coerce x)
        (_, Just y) -> throw $ DuplicateDefns (coerce y)

    findDups :: Ord k => Map k v -> Map k v -> Maybe k
    findDups m1 m2 = case mode of
      -- In standalone mode, we can't have any duplicate definitions.
      Standalone -> listToMaybe . M.keys $ M.intersection m1 m2

      -- In REPL mode, later definitions override earlier ones.
      _          -> Nothing
------------------------------------------------------------
-- Module resolution
------------------------------------------------------------

-- | A data type indicating where we should look for Disco modules to
--   be loaded.
data Resolver
  = FromDir FilePath          -- ^ Load only from a specific directory     (:load)
  | FromCwdOrStdlib           -- ^ Load from current working dir or stdlib (import at REPL)
  | FromDirOrStdlib FilePath  -- ^ Load from specific dir or stdlib        (import in file)

-- | Add the possibility of loading imports from the stdlib.  For
--   example, this is what we want to do after a user loads a specific
--   file using `:load` (for which we will NOT look in the stdlib),
--   but then we need to recursively load modules which it imports
--   (which may either be in the stdlib, or the same directory as the
--   `:load`ed module).
withStdlib :: Resolver -> Resolver
withStdlib (FromDir fp) = FromDirOrStdlib fp
withStdlib r            = r

-- | Given (possibly) a directory and a module name, relavent
--   directories are searched for the file containing the provided
--   module name. If a directory is provided, look only in the
--   specific given directory.  If the directory is @Nothing@, then
--   Disco searches for the module first in the standard library
--   directory (lib), and then in the directory passed in to
--   resolveModule.  Returns Nothing if no module with the given name
--   could be found.
resolveModule :: Member (Embed IO) r => Resolver -> ModName -> Sem r (Maybe FilePath)
resolveModule resolver modname = do
  datadir <- liftIO getDataDir
  let searchPath =
        case resolver of
          FromDir dir         -> [dir]
          FromCwdOrStdlib     -> [datadir, "."]
          FromDirOrStdlib dir -> [datadir, dir]
  let fps = map (</> replaceExtension modname "disco") searchPath
  fexists <- liftIO $ filterM doesFileExist fps
  case fexists of
    []     -> return Nothing
    (fp:_) -> return $ Just fp
