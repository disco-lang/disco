
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Monad
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- XXX change module name?
-- XXX update this comment
-- Definition of the TCM monad used during typechecking and related
-- capabilities + utilities.
--
-----------------------------------------------------------------------------

module Disco.Typecheck.Monad where

import           Disco.Effects.Fresh
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Writer
import           Unbound.Generics.LocallyNameless (Name, bind, string2Name)

import qualified Data.Map                         as M
import           Data.Tuple                       (swap)
import           Prelude                          hiding (lookup)

import           Disco.AST.Surface
import           Disco.AST.Typed                  (QName (..))
import           Disco.Context
import           Disco.Syntax.Prims
import           Disco.Typecheck.Constraints
import           Disco.Typecheck.Solve
import           Disco.Types

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | A typing context is a mapping from term names to types.
type TyCtx = Ctx Term PolyType

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | Potential typechecking errors.
data TCError
  = Unbound (Name Term)    -- ^ Encountered an unbound variable
  | NoType  (Name Term)    -- ^ No type is specified for a definition
  | NotCon Con Term Type   -- ^ The type of the term should have an
                           --   outermost constructor matching Con, but
                           --   it has type 'Type' instead
  | EmptyCase              -- ^ Case analyses cannot be empty.
  | PatternType Pattern Type  -- ^ The given pattern should have the type, but it doesn't.
  | DuplicateDecls (Name Term)  -- ^ Duplicate declarations.
  | DuplicateDefns (Name Term)  -- ^ Duplicate definitions.
  | DuplicateTyDefns String -- ^ Duplicate type definitions.
  | CyclicTyDef String     -- ^ Cyclic type definition.
  | NumPatterns            -- ^ # of patterns does not match type in definition
  | NoLub Type Type        -- ^ No least upper bound.
  | NoNeg Type             -- ^ Type can't support negation.
  | NoSearch Type          -- ^ Type can't be quantified over.
  | Unsolvable SolveError  -- ^ The constraint solver couldn't find a solution.
  | NotTyDef String        -- ^ An undefined type name was used.
  | NoTWild                -- ^ Wildcards are not allowed in terms.
  | CantInferPrim Prim     -- ^ Can't infer the type of some prims
  | NotEnoughArgs Con      -- ^ Not enough arguments provided to type constructor.
  | TooManyArgs Con        -- ^ Too many arguments provided to type constructor.
  | UnboundTyVar (Name Type) -- ^ Unbound type variable
  | NoPolyRec String [String] [Type] -- ^ Polymorphic recursion is not allowed
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
-- Constraints
------------------------------------------------------------

-- | Emit a constraint.
constraint :: Member (Writer Constraint) r => Constraint -> Sem r ()
constraint = tell

-- | Emit a list of constraints.
constraints :: Member (Writer Constraint) r => [Constraint] -> Sem r ()
constraints = constraint . cAnd

-- | Close over the current constraint with a forall.
forAll :: Member (Writer Constraint) r => [Name Type] -> Sem r a -> Sem r a
forAll nms = censor (CAll . bind nms)

-- | Run a computation that generates constraints, returning the
--   generated 'Constraint' along with the output. Note that this
--   locally dispatches the constraint writer effect.
--
--   This function is somewhat low-level; typically you should use
--   'solve' instead, which also solves the generated constraints.
withConstraint :: Sem (Writer Constraint ': r) a -> Sem r (a, Constraint)
withConstraint = fmap swap . runWriter

-- | Run a computation and solve its generated constraint, returning
--   the resulting substitution (or failing with an error).  Note that
--   this locally dispatches the constraint writer effect.
solve :: Members '[Reader TyDefCtx, Error TCError] r => Sem (Writer Constraint ': r) a -> Sem r (a, S)
solve m = do
  (a, c) <- withConstraint m
  tds <- ask @TyDefCtx
  case runSolveM . solveConstraint tds $ c of
    Left err -> throw (Unsolvable err)
    Right s  -> return (a, s)

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | Look up the type of a variable in the context.  Throw an "unbound
--   variable" error if it is not found.
lookupTy ::
  Members '[Reader TyCtx, Error TCError] r
  => Name Term -> Sem r PolyType
lookupTy x = lookup x >>= maybe (throw (Unbound x)) return

-- | Look up the definition of a named type.  Throw a 'NotTyDef' error
--   if it is not found.
lookupTyDefn ::
  Members '[Reader TyDefCtx, Error TCError] r
  => String -> [Type] -> Sem r Type
lookupTyDefn x args = do
  d <- ask @TyDefCtx
  case M.lookup x d of
    Nothing                 -> throw (NotTyDef x)
    Just (TyDefBody _ body) -> return $ body args

-- | Run a subcomputation with an extended type definition context.
withTyDefns :: Member (Reader TyDefCtx) r => TyDefCtx -> Sem r a -> Sem r a
withTyDefns tyDefnCtx = local (M.union tyDefnCtx)

------------------------------------------------------------
-- Fresh name generation
------------------------------------------------------------

-- | Generate a type variable with a fresh name.
freshTy :: Member Fresh r => Sem r Type
freshTy = TyVar <$> fresh (string2Name "a")

-- | Generate a fresh variable as an atom.
freshAtom :: Member Fresh r => Sem r Atom
freshAtom = AVar . U <$> fresh (string2Name "c")
