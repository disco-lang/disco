-- |
-- Module      :  Disco.Typecheck.Util
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Definition of type contexts, type errors, and various utilities
-- used during type checking.
module Disco.Typecheck.Util where

import Disco.Effects.Fresh
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader
import Polysemy.Writer
import Unbound.Generics.LocallyNameless (Name, bind, string2Name)

import qualified Data.Map as M
import Data.Tuple (swap)
import Prelude hiding (lookup)

import Disco.AST.Surface
import Disco.Context
import Disco.Messages
import Disco.Names (ModuleName, QName)
import Disco.Typecheck.Constraints
import Disco.Typecheck.Solve
import Disco.Types

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | A typing context is a mapping from term names to types.
type TyCtx = Ctx Term PolyType

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | A typechecking error, wrapped up together with the name of the
--   thing that was being checked when the error occurred.
data LocTCError = LocTCError (Maybe (QName Term)) TCError
  deriving (Show)

-- | Wrap a @TCError@ into a @LocTCError@ with no explicit provenance
--   information.
noLoc :: TCError -> LocTCError
noLoc = LocTCError Nothing

-- | Potential typechecking errors.
data TCError
  = -- | Encountered an unbound variable
    Unbound (Name Term)
  | -- | Encountered an ambiguous name.
    Ambiguous (Name Term) [ModuleName]
  | -- | No type is specified for a definition
    NoType (Name Term)
  | -- | The type of the term should have an
    --   outermost constructor matching Con, but
    --   it has type 'Type' instead
    NotCon Con Term Type
  | -- | Case analyses cannot be empty.
    EmptyCase
  | -- | The given pattern should have the type, but it doesn't.
    -- instead it has a kind of type given by the Con.
    PatternType Con Pattern Type
  | -- | Duplicate declarations.
    DuplicateDecls (Name Term)
  | -- | Duplicate definitions.
    DuplicateDefns (Name Term)
  | -- | Duplicate type definitions.
    DuplicateTyDefns String
  | -- | Cyclic type definition.
    CyclicTyDef String
  | -- | # of patterns does not match type in definition
    NumPatterns
  | -- | Duplicate variable in a pattern
    NonlinearPattern Pattern (Name Term)
  | -- | Type can't be quantified over.
    NoSearch Type
  | -- | The constraint solver couldn't find a solution.
    Unsolvable SolveError
  | -- | An undefined type name was used.
    NotTyDef String
  | -- | Wildcards are not allowed in terms.
    NoTWild
  | -- | Not enough arguments provided to type constructor.
    NotEnoughArgs Con
  | -- | Too many arguments provided to type constructor.
    TooManyArgs Con
  | -- | Unbound type variable, together with suggested edits
    UnboundTyVar (Name Type) [String]
  | -- | Polymorphic recursion is not allowed
    NoPolyRec String [String] [Type]
  | -- | Not an error.  The identity of the
    --   @Monoid TCError@ instance.
    NoError
  deriving (Show)

instance Semigroup TCError where
  _ <> r = r

-- | 'TCError' is a monoid where we simply discard the first error.
instance Monoid TCError where
  mempty = NoError
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

-- | Run two constraint-generating actions and combine the constraints
--   via disjunction.
cOr :: Members '[Writer Constraint] r => Sem r () -> Sem r () -> Sem r ()
cOr m1 m2 = do
  (c1, _) <- censor (const CTrue) (listen m1)
  (c2, _) <- censor (const CTrue) (listen m2)
  constraint $ COr [c1, c2]

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
solve ::
  Members '[Reader TyDefCtx, Error TCError, Output (Message ann)] r =>
  Sem (Writer Constraint ': r) a ->
  Sem r (a, S)
solve m = do
  (a, c) <- withConstraint m
  res <- runSolve . inputToReader . solveConstraint $ c
  case res of
    Left e -> throw (Unsolvable e)
    Right s -> return (a, s)

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | Look up the definition of a named type.  Throw a 'NotTyDef' error
--   if it is not found.
lookupTyDefn ::
  Members '[Reader TyDefCtx, Error TCError] r =>
  String ->
  [Type] ->
  Sem r Type
lookupTyDefn x args = do
  d <- ask @TyDefCtx
  case M.lookup x d of
    Nothing -> throw (NotTyDef x)
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
