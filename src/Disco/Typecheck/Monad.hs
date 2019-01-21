{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Monad
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Definition of the TCM monad used during typechecking and related
-- utilities.
--
-----------------------------------------------------------------------------

module Disco.Typecheck.Monad where

import           GHC.Generics                     (Generic)

import           Unbound.Generics.LocallyNameless

import           Control.Lens                     (makeLenses)
import           Control.Monad.Except
import           Control.Monad.Fail               (MonadFail)
import qualified Control.Monad.Fail               as Fail
import           Control.Monad.RWS
import qualified Data.Map                         as M
import           Prelude                          hiding (lookup)

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Typecheck.Constraints
import           Disco.Typecheck.Solve
import           Disco.Types

------------------------------------------------------------
-- Definitions and contexts
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

-- | A map from type names to their corresponding definitions.
type TyDefCtx = M.Map String Type

-- | A typing context is a mapping from term names to types.
type TyCtx = Ctx Term Sigma

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

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | Potential typechecking errors.
data TCError
  = Unbound (Name Term)    -- ^ Encountered an unbound variable
  | NotCon Con Term Type   -- ^ The term should have an outermost constructor matching
                           --   matching Con, but it has type 'Type' instead
  | EmptyCase              -- ^ Case analyses cannot be empty.
  | PatternType Pattern Type  -- ^ The given pattern should have the type, but it doesn't.
  | DuplicateDecls (Name Term)  -- ^ Duplicate declarations.
  | DuplicateDefns (Name Term)  -- ^ Duplicate definitions.
  | DuplicateTyDefns String -- ^ Duplicate type definitions.
  | CyclicTyDef String     -- ^ Cyclic type definition.
  | NumPatterns            -- ^ # of patterns does not match type in definition
  | NoLub Type Type        -- ^ No least upper bound.
  | NoNeg Type             -- ^ Type can't support negation.
  | Unsolvable SolveError  -- ^ The constraint solver couldn't find a solution.
  | NotTyDef String        -- ^ An undefined type name was used.
  | NoTWild                -- ^ Wildcards are not allowed in terms.
  | Failure String         -- ^ Generic failure.
  | NoError                -- ^ Not an error.  The identity of the
                           --   @Monoid TCError@ instance.
  deriving Show

instance Semigroup TCError where
  _ <> r = r

-- | 'TCError' is a monoid where we simply discard the first error.
instance Monoid TCError where
  mempty  = NoError
  mappend = (<>)

------------------------------------------------------------
-- TCM monad definition
------------------------------------------------------------

-- | Type checking monad. Maintains a locally-scoped context of
--   variables and their types and a read-write map of type
--   definitions; collects constraints; can throw @TCError@s; and
--   can generate fresh names.
type TCM = RWST TyCtx Constraint TyDefCtx (ExceptT TCError FreshM)

-- This is an orphan instance, but we can't very well add it to either
-- 'containers' or 'unbound-generic'.
instance (Monoid w, Fresh m) => Fresh (RWST r w s m) where
  fresh = lift . fresh

------------------------------------------------------------
-- Running
------------------------------------------------------------

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError (a, Constraint)
runTCM = runFreshM . runExceptT . (\m -> fmap (\(a,_,c) -> (a,c)) (runRWST m emptyCtx M.empty))

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the result of the computation.
evalTCM :: TCM a -> Either TCError a
evalTCM = fmap (\(a,_) -> a) . runTCM

------------------------------------------------------------
-- Constraints
------------------------------------------------------------

-- | Add a constraint.
constraint :: Constraint -> TCM ()
constraint = tell

-- | Add a list of constraints.
constraints :: [Constraint] -> TCM ()
constraints = constraint . cAnd

-- | Close over the current constraint with a forall.
forAll :: [Name Type] -> TCM a -> TCM a
forAll nms = censor (CAll . bind nms)

-- | Run a 'TCM' computation, returning the generated 'Constraint'
--   along with the output, and reset the 'Constraint' of the resulting
--   computation to 'mempty'.
withConstraint :: TCM a -> TCM (a, Constraint)
withConstraint = censor (const mempty) . listen

-- | Run a 'TCM' computation and solve its generated constraint,
--   returning the resulting substitution (or failing with an error).
--   The resulting TCM computation generates the empty constraint.
solve :: TCM a -> TCM (a, S)
solve m = do
  (a, c) <- withConstraint m
  tds <- get
  case runSolveM . solveConstraint tds $ c of
    Left err -> throwError (Unsolvable err)
    Right s  -> return (a, s)

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | Look up the type of a variable in the context.  Throw an "unbound
--   variable" error if it is not found.
lookupTy :: Name Term -> TCM Sigma
lookupTy x = lookup x >>= maybe (throwError (Unbound x)) return

-- | Look up the definition of a named type.  Throw a 'NotTyDef' error
--   if it is not found.
lookupTyDefn :: String -> TCM Type
lookupTyDefn x = do
  d <- get
  case M.lookup x d of
    Nothing -> throwError (NotTyDef x)
    Just ty -> return ty

withTyDefns :: TyDefCtx -> TCM a -> TCM a
withTyDefns tyDefnCtx m = do
  oldTyDefs <- get
  modify (M.union tyDefnCtx)
  a <- m
  put oldTyDefs
  return a

------------------------------------------------------------
-- Fresh name generation
------------------------------------------------------------

-- | Generate a type variable with a fresh name.
freshTy :: TCM Type
freshTy = TyVar <$> fresh (string2Name "a")


