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
import Disco.Effects.Fresh
import Disco.Effects.LFresh (LFresh)
import Disco.Error
import Disco.Messages
import Disco.Names (ModuleName, QName)
import Disco.Pretty
import Disco.Typecheck.Constraints
import Disco.Typecheck.Solve
import Disco.Types
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader
import Polysemy.Writer
import Unbound.Generics.LocallyNameless (Name, bind, string2Name)
import Prelude hiding (lookup, (<>))

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
  deriving (Show)

------------------------------------------------------------
-- Error reporting
------------------------------------------------------------

-- [X] Step 1: nice error messages, make sure all are tested
-- [X] Step 2: link to wiki/website with more info on errors!
-- [ ] Step 3: improve error messages according to notes below
-- [ ] Step 4: get it to return multiple error messages
-- [ ] Step 5: save parse locations, display with errors
reportLocTCError :: LocTCError -> Sem r DiscoError
reportLocTCError (LocTCError _ tcErr) = reportTCError tcErr -- XXX

reportTCError :: TCError -> Sem r DiscoError
reportTCError tce = runPretty $ case tce of
  -- XXX include some potential misspellings along with Unbound
  --   see https://github.com/disco-lang/disco/issues/180
  Unbound x ->
    tcErrorRpt
      ("There is nothing named" <+> pretty' x <> ".")
      []
      [RTD "unbound"]
  Ambiguous x ms ->
    tcErrorRpt
      ( vcat
          [ "The name" <+> pretty' x <+> "is ambiguous. It could refer to:"
          , nest 2 (vcat . map (\m -> pretty' m <> "." <> pretty' x) $ ms)
          ]
      )
      []
      [RTD "ambiguous"]
  NoType x ->
    tcErrorRpt
      ("The definition of" <+> pretty' x <+> "must have an accompanying type signature.")
      [ "Try writing something like '"
          <> pretty' x
          <+> ": Int' (or whatever the type of"
          <+> pretty' x
          <+> "should be) first."
      ]
      [RTD "missingtype"]
  NotCon c t ty ->
    tcErrorRpt
      ( vcat
          [ "The expression"
          , nest 2 $ pretty' t
          , "must have both a" <+> conWord c <+> "type and also the incompatible type"
          , nest 2 $ pretty' ty <> "."
          ]
      )
      []
      [RTD "notcon"]
  EmptyCase ->
    tcErrorRpt
      "Empty case expressions {? ?} are not allowed."
      []
      [RTD "empty-case"]
  PatternType c pat ty ->
    tcErrorRpt
      ( vcat
          [ "The pattern"
          , nest 2 $ pretty' pat
          , "is supposed to have type"
          , nest 2 $ pretty' ty <> ","
          , "but instead it has a" <+> conWord c <+> "type."
          ]
      )
      []
      [RTD "pattern-type"]
  DuplicateDecls x ->
    tcErrorRpt
      ("Duplicate type signature for" <+> pretty' x <> ".")
      []
      [RTD "dup-sig"]
  DuplicateDefns x ->
    tcErrorRpt
      ("Duplicate definition for" <+> pretty' x <> ".")
      []
      [RTD "dup-def"]
  DuplicateTyDefns s ->
    tcErrorRpt
      ("Duplicate definition for type" <+> text s <> ".")
      []
      [RTD "dup-tydef"]
  -- XXX include all types involved in the cycle.
  CyclicTyDef s ->
    tcErrorRpt
      ("Cyclic type definition for" <+> text s <> ".")
      []
      [RTD "cyc-ty"]
  -- XXX lots more info!  & Split into several different errors.
  NumPatterns ->
    tcErrorRpt
      "Number of arguments does not match."
      []
      [RTD "num-args"]
  NonlinearPattern p x ->
    tcErrorRpt
      ("Pattern" <+> pretty' p <+> "contains duplicate variable" <+> pretty' x <> ".")
      []
      [RTD "nonlinear"]
  NoSearch ty ->
    tcErrorRpt
      ( vcat
          [ "Error: the type"
          , nest 2 $ pretty' ty
          , "is not searchable (i.e. it cannot be used in a forall)."
          ]
      )
      []
      [RTD "no-search"]
  Unsolvable solveErr -> reportSolveError solveErr
  -- XXX maybe include close edit-distance alternatives?
  NotTyDef s ->
    tcErrorRpt
      ("There is no built-in or user-defined type named '" <> text s <> "'.")
      []
      [RTD "no-tydef"]
  NoTWild ->
    tcErrorRpt
      "Wildcards (_) are not allowed in expressions."
      []
      [RTD "wildcard-expr"]
  -- XXX say how many are expected, how many there were, what the actual arguments were?
  -- XXX distinguish between built-in and user-supplied type constructors in the error
  --     message?
  NotEnoughArgs con ->
    tcErrorRpt
      ("Not enough arguments for the type '" <> pretty' con <> "'.")
      []
      [RTD "num-args-type"]
  TooManyArgs con ->
    tcErrorRpt
      ("Too many arguments for the type '" <> pretty' con <> "'.")
      []
      [RTD "num-args-type"]
  -- XXX Mention the definition in which it was found, suggest adding the variable
  --     as a parameter
  UnboundTyVar v ->
    tcErrorRpt
      ("Unknown type variable '" <> pretty' v <> "'.")
      []
      [RTD "unbound-tyvar"]
  NoPolyRec s ss tys ->
    tcErrorRpt
      ( vcat
          [ "In the definition of " <> text s <> parens (intercalate "," (map text ss)) <> ": recursive occurrences of" <+> text s <+> "may only have type variables as arguments."
          , nest
              2
              ( text s <> parens (intercalate "," (map pretty' tys)) <+> "does not follow this rule."
              )
          ]
      )
      []
      [RTD "no-poly-rec"]
 where
  tcErrorRpt expl hints rdg = do
    expl' <- expl
    hints' <- sequence hints
    return $
      DiscoError
        { errHeadline = "Type error"
        , errKind = TypeCheckErr
        , errExplanation = expl'
        , errHints = hints'
        , errReading = rdg
        }

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
