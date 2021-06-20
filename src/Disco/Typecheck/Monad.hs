{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
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
-- XXX fix documentation.  Not just about TCM monad anymore, uses
-- capability style.
--
-----------------------------------------------------------------------------

module Disco.Typecheck.Monad where

import           GHC.Exts                         (Proxy#, proxy#)
import           GHC.Generics                     (Generic)

import           Unbound.Generics.LocallyNameless

import qualified Capability.Constraints           as CC
import           Capability.Error
import           Capability.Reader
import           Capability.Sink
import           Capability.Source
import           Capability.Writer
import           Control.Monad.Except             (ExceptT (..))
import qualified Control.Monad.Except             as CME
import           Control.Monad.State              (StateT (..))
import qualified Data.Map                         as M
import           Prelude                          hiding (lookup)

import           Data.Bifunctor                   (second)

import           Disco.AST.Surface
import           Disco.Capability
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
-- TCM monad definition
------------------------------------------------------------

data TCMState = TCMState
  { tcmTyCtx       :: TyCtx
  , tcmConstraints :: Constraint
  , tcmTyDefCtx    :: TyDefCtx
  }
  deriving (Generic)

initTCMState :: TCMState
initTCMState = TCMState
  { tcmTyCtx       = emptyCtx
  , tcmConstraints = CTrue
  , tcmTyDefCtx    = M.empty
  }

-- | Type checking monad. Maintains locally-scoped contexts of
--   variables with types and of type name definitions; collects
--   constraints; can throw @TCError@s; and can generate fresh names.
newtype TCM a = TCM { unTCM :: StateT TCMState (ExceptT TCError FreshM) a }
  deriving (Functor, Applicative, Monad, Fresh, CME.MonadError TCError)
  deriving (HasReader "tyctx" TyCtx, HasSource "tyctx" TyCtx) via
    (ReadStatePure
    (Rename "tcmTyCtx"
    (Field "tcmTyCtx" ()
    (MonadState
    (StateT TCMState (ExceptT TCError FreshM))))))
  deriving (HasReader "tydefctx" TyDefCtx, HasSource "tydefctx" TyDefCtx) via
    (ReadStatePure
    (Rename "tcmTyDefCtx"
    (Field "tcmTyDefCtx" ()
    (MonadState
    (StateT TCMState (ExceptT TCError FreshM))))))
  deriving (HasWriter "constraints" Constraint, HasSink "constraints" Constraint) via
    (WriterLog
    (Rename "tcmConstraints"
    (Field "tcmConstraints" ()
    (MonadState
    (StateT TCMState (ExceptT TCError FreshM))))))
  deriving (HasThrow "tcerr" TCError, HasCatch "tcerr" TCError) via
    (MonadError
    (StateT TCMState (ExceptT TCError FreshM)))

-- -- This is an orphan instance, but we can't very well add it to either
-- -- 'containers' or 'unbound-generic'.
-- instance (Monoid w, Fresh m) => Fresh (StateT s m) where
--   fresh = lift . fresh

type instance TypeOf _ "constraints" = Constraint
type instance TypeOf _ "tyctx"       = TyCtx
type instance TypeOf _ "tydefctx"    = TyDefCtx
type instance TypeOf _ "tcerr"       = TCError

------------------------------------------------------------
-- Running
------------------------------------------------------------

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError (a, Constraint)
runTCM = runFreshM . CME.runExceptT . fmap (second tcmConstraints) . (`runStateT` initTCMState) . unTCM

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the result of the computation.
evalTCM :: TCM a -> Either TCError a
evalTCM = fmap fst . runTCM

------------------------------------------------------------
-- Constraints
------------------------------------------------------------

-- | Emit a constraint.
constraint :: Has '[Wr "constraints"] m => Constraint -> m ()
constraint = tell @"constraints"

-- | Emit a list of constraints.
constraints :: Has '[Wr "constraints"] m => [Constraint] -> m ()
constraints = constraint . cAnd

-- XXX copied from mtl, should be in capability library?  submitted
-- PR: https://github.com/tweag/capability/pull/94 PR was merged.  If
-- this is eventually released, we can depend on a newer version of
-- capability and delete the following two functions
censor_ :: forall k (tag :: k) w m a. HasWriter tag w m => Proxy# tag -> (w -> w) -> m a -> m a
censor_ tag f m = pass_ tag $ (,f) <$> m

censor :: forall tag w m a. HasWriter tag w m => (w -> w) -> m a -> m a
censor = censor_ (proxy# @tag)
{-# INLINE censor #-}

  -- Wow, that took me a LONG time to figure out.  Was initially
  -- getting an error when calling censor @"constraints":
  --
  --     • Expected a type, but
  --       ‘"constraints"’ has kind
  --       ‘ghc-prim-0.6.1:GHC.Types.Symbol’
  --     • In the type ‘"constraints"’
  --       In the expression: censor @"constraints" (CAll . bind nms)
  --       In an equation for ‘forAll’:
  --           forAll nms = censor @"constraints" (CAll . bind nms)
  --
  -- The problem was that my type signature for censor_ looked like this:
  --
  --   censor_ :: forall tag w m a. HasWriter tag w m => ...
  --
  -- but this means that 'tag' was being inferred to have kind '*'.
  -- The solution is to explicitly declare 'tag' to be
  -- kind-polymorphic, by declaring a kind variable k bound by the
  -- forall and putting a kind signature on tag.  Note that k does not
  -- count as the first argument with TypeApplications.  ...UNLESS we
  -- have enabled TypeInType!! Then it does count!
  --
  -- Also, it seems that for some reason both censor_ and censor are
  -- required.  It seems like we could define
  --
  --   censor :: forall k (tag :: k) w m a. HasWriter tag w m => (w -> w) -> m a -> m a
  --   censor f m = pass @tag $ (,f) <$> m
  --
  -- or perhaps even
  --
  --   censor f m = pass_ (proxy# @tag) $ (,f) <$> m
  --
  -- but this doesn't work; we start getting the same "Expected a type"
  -- errors at call sites again.

-- | Close over the current constraint with a forall.
forAll :: Has '[Wr "constraints"] m => [Name Type] -> m a -> m a
forAll nms = censor @"constraints" (CAll . bind nms)

-- XXX fix documentation (references to TCM)
-- | Run a 'TCM' computation, returning the generated 'Constraint'
--   along with the output, and reset the 'Constraint' of the resulting
--   computation to 'mempty'.
withConstraint :: Has '[Wr "constraints"] m => m a -> m (a, Constraint)
withConstraint = censor @"constraints" (const mempty) . listen @"constraints"

-- | Run a 'TCM' computation and solve its generated constraint,
--   returning the resulting substitution (or failing with an error).
--   The resulting TCM computation generates the empty constraint.
solve :: Has '[Wr "constraints", Rd "tydefctx", Th "tcerr"] m => m a -> m (a, S)
solve m = do
  (a, c) <- withConstraint m
  tds <- ask @"tydefctx"
  case runSolveM . solveConstraint tds $ c of
    Left err -> throw @"tcerr" (Unsolvable err)
    Right s  -> return (a, s)

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | Look up the type of a variable in the context.  Throw an "unbound
--   variable" error if it is not found.
lookupTy ::
  ( HasReader' "tyctx" m, HasThrow' "tcerr" m )
  => Name Term -> m PolyType
lookupTy x = lookup @"tyctx" x >>= maybe (throw @"tcerr" (Unbound x)) return

-- | Look up the definition of a named type.  Throw a 'NotTyDef' error
--   if it is not found.
lookupTyDefn ::
  ( HasReader' "tydefctx" m, HasThrow' "tcerr" m )
  => String -> [Type] -> m Type
lookupTyDefn x args = do
  d <- ask @"tydefctx"
  case M.lookup x d of
    Nothing                 -> throw @"tcerr" (NotTyDef x)
    Just (TyDefBody _ body) -> return $ body args

withTyDefns :: HasReader' "tydefctx" m => TyDefCtx -> m a -> m a
withTyDefns tyDefnCtx = local @"tydefctx" (M.union tyDefnCtx)

------------------------------------------------------------
-- Fresh name generation
------------------------------------------------------------

-- | Generate a type variable with a fresh name.
freshTy :: Fresh m => m Type
freshTy = TyVar <$> fresh (string2Name "a")

-- | Generate a fresh variable as an atom.
freshAtom :: Fresh m => m Atom
freshAtom = AVar . U <$> fresh (string2Name "c")
