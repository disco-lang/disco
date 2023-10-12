{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Disco.Typecheck.Util
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Definition of type contexts, type errors, and various utilities
-- used during type checking.
module Disco.Typecheck.Util where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Data.Tuple (swap)
import Disco.AST.Surface
import Disco.Context
import Disco.Effects.LFresh (LFresh)
import Disco.Error
import Disco.Messages
import Disco.Names (ModuleName, QName)
import Disco.Pretty (Doc, PA)
import Disco.Typecheck.Constraints
import Disco.Typecheck.Solve
import Disco.Types
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader
import Polysemy.Writer
import Unbound.Generics.LocallyNameless (Name, bind, string2Name)
import Prelude hiding (lookup)

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
  = -- | Encountered an unbound variable.  The offending variable
    --   together with some suggested in-scope names with small edit
    --   distance.
    Unbound (Name Term) [String]
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
-- Error reporting
------------------------------------------------------------

reportTCError :: Members '[Reader PA, LFresh] r => TCError -> Sem r DiscoError
reportTCError tce = do
  return
    $ DiscoError
      { errHeadline = "Type error"
      , errKind = TypeCheckErr
      , errExplanation = undefined
      , errHints = undefined
      , errReading = undefined
      }

-- [X] Step 1: nice error messages, make sure all are tested
-- [X] Step 2: link to wiki/website with more info on errors!
-- [ ] Step 3: improve error messages according to notes below
-- [ ] Step 4: get it to return multiple error messages
-- [ ] Step 5: save parse locations, display with errors
prettyTCError :: Members '[Reader PA, LFresh] r => TCError -> Sem r Doc
prettyTCError = \case
  -- XXX include some potential misspellings along with Unbound
  --   see https://github.com/disco-lang/disco/issues/180
  Unbound x ->
    vcat
      [ "Error: there is nothing named" <+> pretty' x <> "."
      , rtd "unbound"
      ]
  Ambiguous x ms ->
    vcat
      [ "Error: the name" <+> pretty' x <+> "is ambiguous. It could refer to:"
      , nest 2 (vcat . map (\m -> pretty' m <> "." <> pretty' x) $ ms)
      , rtd "ambiguous"
      ]
  NoType x ->
    vcat
      [ "Error: the definition of" <+> pretty' x <+> "must have an accompanying type signature."
      , "Try writing something like '"
          <> pretty' x
          <+> ": Int' (or whatever the type of"
          <+> pretty' x
          <+> "should be) first."
      , rtd "missingtype"
      ]
  NotCon c t ty ->
    vcat
      [ "Error: the expression"
      , nest 2 $ pretty' t
      , "must have both a" <+> conWord c <+> "type and also the incompatible type"
      , nest 2 $ pretty' ty <> "."
      , rtd "notcon"
      ]
  EmptyCase ->
    vcat
      [ "Error: empty case expressions {? ?} are not allowed."
      , rtd "empty-case"
      ]
  PatternType c pat ty ->
    vcat
      [ "Error: the pattern"
      , nest 2 $ pretty' pat
      , "is supposed to have type"
      , nest 2 $ pretty' ty <> ","
      , "but instead it has a" <+> conWord c <+> "type."
      , rtd "pattern-type"
      ]
  DuplicateDecls x ->
    vcat
      [ "Error: duplicate type signature for" <+> pretty' x <> "."
      , rtd "dup-sig"
      ]
  DuplicateDefns x ->
    vcat
      [ "Error: duplicate definition for" <+> pretty' x <> "."
      , rtd "dup-def"
      ]
  DuplicateTyDefns s ->
    vcat
      [ "Error: duplicate definition for type" <+> text s <> "."
      , rtd "dup-tydef"
      ]
  -- XXX include all types involved in the cycle.
  CyclicTyDef s ->
    vcat
      [ "Error: cyclic type definition for" <+> text s <> "."
      , rtd "cyc-ty"
      ]
  -- XXX lots more info!  & Split into several different errors.
  NumPatterns ->
    vcat
      [ "Error: number of arguments does not match."
      , rtd "num-args"
      ]
  NonlinearPattern p x ->
    vcat
      [ "Error: pattern" <+> pretty' p <+> "contains duplicate variable" <+> pretty' x <> "."
      , rtd "nonlinear"
      ]
  NoSearch ty ->
    vcat
      [ "Error: the type"
      , nest 2 $ pretty' ty
      , "is not searchable (i.e. it cannot be used in a forall)."
      , rtd "no-search"
      ]
  Unsolvable solveErr -> prettySolveError solveErr
  -- XXX maybe include close edit-distance alternatives?
  NotTyDef s ->
    vcat
      [ "Error: there is no built-in or user-defined type named '" <> text s <> "'."
      , rtd "no-tydef"
      ]
  NoTWild ->
    vcat
      [ "Error: wildcards (_) are not allowed in expressions."
      , rtd "wildcard-expr"
      ]
  -- XXX say how many are expected, how many there were, what the actual arguments were?
  -- XXX distinguish between built-in and user-supplied type constructors in the error
  --     message?
  NotEnoughArgs con ->
    vcat
      [ "Error: not enough arguments for the type '" <> pretty' con <> "'."
      , rtd "num-args-type"
      ]
  TooManyArgs con ->
    vcat
      [ "Error: too many arguments for the type '" <> pretty' con <> "'."
      , rtd "num-args-type"
      ]
  -- XXX Mention the definition in which it was found, suggest adding the variable
  --     as a parameter
  UnboundTyVar v ->
    vcat
      [ "Error: Unknown type variable '" <> pretty' v <> "'."
      , rtd "unbound-tyvar"
      ]
  NoPolyRec s ss tys ->
    vcat
      [ "Error: in the definition of " <> text s <> parens (intercalate "," (map text ss)) <> ": recursive occurrences of" <+> text s <+> "may only have type variables as arguments."
      , nest
          2
          ( text s <> parens (intercalate "," (map pretty' tys)) <+> "does not follow this rule."
          )
      , rtd "no-poly-rec"
      ]
  NoError -> empty

conWord :: Con -> Sem r Doc
conWord = \case
  CArr -> "function"
  CProd -> "pair"
  CSum -> "sum"
  CSet -> "set"
  CBag -> "bag"
  CList -> "list"
  CContainer _ -> "container"
  CMap -> "map"
  CGraph -> "graph"
  CUser s -> text s

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
orElse :: Members '[Writer Constraint] r => Sem r () -> Sem r () -> Sem r ()
orElse m1 m2 = do
  (c1, _) <- censor (const CTrue) (listen m1)
  (c2, _) <- censor (const CTrue) (listen m2)
  constraint $ COr (c1 :| [c2])

-- | Run a computation that generates constraints, returning the
--   generated 'Constraint' along with the output. Note that this
--   locally dispatches the constraint writer effect.
--
--   This function is somewhat low-level; typically you should use
--   'solve' instead, which also solves the generated constraints.
withConstraint :: Sem (Writer Constraint ': r) a -> Sem r (a, Constraint)
withConstraint = fmap swap . runWriter

-- | Run a computation and solve its generated constraint, returning
--   up to the requested number of possible resulting substitutions
--   (or failing with an error).  Note that this locally dispatches
--   the constraint writer and solution limit effects.
solve ::
  Members '[Reader TyDefCtx, Error TCError, Output (Message ann)] r =>
  Int ->
  Sem (Writer Constraint ': r) a ->
  Sem r (a, NonEmpty S)
solve lim m = do
  (a, c) <- withConstraint m
  res <- runSolve (SolutionLimit lim) . inputToReader . solveConstraint $ c
  case res of
    Left e -> throw (Unsolvable e)
    Right ss -> return (a, ss)

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
