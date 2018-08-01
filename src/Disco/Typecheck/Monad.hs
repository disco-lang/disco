{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

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

import           Control.Lens                     (makeLenses, use, (%=), (%~))
import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Bifunctor                   (second)
import           Data.Coerce
import qualified Data.List                        as L
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

-- | A context of definitions: a map from names of terms to their
--   (typechecked) definitions, as well as a map of named types.
data DefnCtx = DefnCtx
  { _termDefns :: Ctx ATerm Defn
  , _tyDefns   :: TyDefCtx
  }

makeLenses ''DefnCtx

-- | The initial, empty @DefnCtx@.
emptyDefnCtx :: DefnCtx
emptyDefnCtx = DefnCtx
  { _termDefns = emptyCtx
  , _tyDefns   = M.empty
  }

-- | A typing context is a mapping from term names to types.
type TyCtx = Ctx Term Sigma

-- | Type checking a module yields a value of type ModuleInfo which contains
--   mapping from terms to their relavent documenation, a mapping from terms to
--   properties, and a mapping from terms to their types.
data ModuleInfo = ModuleInfo
  { _docs :: Ctx Term Docs
  , _props :: Ctx ATerm [AProperty]
  , _tys :: TyCtx
  }

instance Monoid ModuleInfo where
  mempty = ModuleInfo emptyCtx emptyCtx emptyCtx
  mappend (ModuleInfo d1 p1 t1) (ModuleInfo d2 p2 t2) =
   case hasDupTerm t1 t2 of
      Nothing -> ModuleInfo (joinCtx d1 d2) (joinCtx p1 p2) (joinCtx t1 t2)
      -- XXX: Needs to throw a TCError instead
      Just t -> error $ "Duplicate term definition:" ++ show t  

    where hasDupTerm :: TyCtx -> TyCtx -> Maybe (Name Term)
          hasDupTerm trm1 trm2 = case L.intersect (M.keys trm1) (M.keys trm2) of
                              [] -> Nothing
                              (x:_) -> Just x

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
  | Unsolvable SolveError
  | NotTyDef String             -- ^ The type is an algebraic data type that was never defined.
  | NoError                -- ^ Not an error.  The identity of the
                           --   @Monoid TCError@ instance.
  deriving Show

-- | 'TCError' is a monoid where we simply discard the first error.
instance Monoid TCError where
  mempty = NoError
  mappend _ r = r

------------------------------------------------------------
-- TCM monad definition
------------------------------------------------------------

-- | Type checking monad. Maintains a locally-scoped context of
--   variables and their types and a read-write context of term and
--   type definitions; collects constraints; can throw @TCError@s; and
--   can generate fresh names.
type TCM = RWST TyCtx Constraint DefnCtx (ExceptT TCError FreshM)

-- This is an orphan instance, but we can't very well add it to either
-- 'containers' or 'unbound-generic'.
instance (Monoid w, Fresh m) => Fresh (RWST r w s m) where
  fresh = lift . fresh

------------------------------------------------------------
-- Running
------------------------------------------------------------

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError (a, DefnCtx, Constraint)
runTCM = runFreshM . runExceptT . (\m -> runRWST m emptyCtx emptyDefnCtx)

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the result of the computation.
evalTCM :: TCM a -> Either TCError a
evalTCM = fmap (\(a,_,_) -> a) . runTCM

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
  tds <- use tyDefns
  case runSolveM . solveConstraint tds $ c of
    Left err -> throwError (Unsolvable err)
    Right s  -> return (a, s)

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | Add a definition to the set of current definitions.
addDefn :: Name Term -> Defn -> TCM ()
addDefn x b = termDefns %= M.insert (coerce x) b

-- | Add a type definition to the set of current type defintions.
addTyDefn :: String -> Type -> TCM ()
addTyDefn x b = tyDefns %= M.insert x b

-- | Extend the current TyDefCtx with a provided tydef context.
extendTyDefs :: TyDefCtx -> TCM a -> TCM a
extendTyDefs newtyctx
  = withRWST (curry (second (tyDefns %~ M.union newtyctx)))

-- | Look up the type of a variable in the context.  Throw an "unbound
--   variable" error if it is not found.
lookupTy :: Name Term -> TCM Sigma
lookupTy x = lookup x >>= maybe (throwError (Unbound x)) return

-- | Look up the definition of a named type.  Throw a 'NotTyDef' error
--   if it is not found.
lookupTyDefn :: String -> TCM Type
lookupTyDefn x = do
  d <- use tyDefns
  case M.lookup x d of
    Nothing -> throwError (NotTyDef x)
    Just ty -> return ty

------------------------------------------------------------
-- Fresh name generation
------------------------------------------------------------

-- | Generate a type variable with a fresh name.
freshTy :: TCM Type
freshTy = TyVar <$> fresh (string2Name "a")


