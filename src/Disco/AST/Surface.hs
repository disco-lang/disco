{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}  -- for Alpha Rational

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Surface
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Abstract syntax trees representing the surface syntax of the Disco
-- language.
--
-----------------------------------------------------------------------------

module Disco.AST.Surface
       ( -- * Modules
         Module(..), TopLevel(..)
         -- ** Documentation
       , Docs, DocMap, DocThing(..), Property
         -- ** Declarations
       , Decl(..), declName, isDefn

         -- * Operators
       , UOp(..), BOp(..), UFixity(..), BFixity(..), OpFixity(..)
       , OpInfo(..), opTable, uopMap, bopMap

         -- * Terms
       , Side(..), Link(..), TyOp(..), Term(..)

         -- * Case expressions and patterns
       , Branch, Guards(..), Guard(..), Pattern(..)
       )
       where

import qualified Data.Map                as M

import           Unbound.LocallyNameless hiding (Fixity)

import           Disco.Types

-- | A module is a list of declarations together with a collection of
--   documentation for top-level names.
data Module = Module [Decl] DocMap
  deriving Show

-- | A @TopLevel@ is either documentation (a 'DocThing') or a
--   declaration ('Decl').
data TopLevel = TLDoc DocThing | TLDecl Decl
  deriving Show

-- | Convenient synonym for a list of 'DocThing's.
type Docs = [DocThing]

-- | A 'DocMap' is a mapping from names to documentation.
type DocMap = M.Map (Name Term) Docs

-- | An item of documentation.
data DocThing
  = DocString     [String]      -- ^ A documentation string, i.e. a block of @||| text@ items
  | DocProperties [Property]    -- ^ A group of examples/properties of the form @!!! property@
  deriving Show

-- | A property is a universally quantified term of the form
--   @forall (v1 : T1) (v2 : T2). term@.
type Property = Bind [(Name Term, Type)] Term

-- | A declaration is either a type declaration or a definition.
data Decl where

  -- | A type declaration, @name : type@.
  DType :: Name Term -> Type -> Decl

  -- | A group of definition clauses of the form @name pat1 .. patn = term@. The
  --   patterns bind variables in the term. For example, @f n (x,y) =
  --   n*x + y@.
  DDefn :: Name Term -> [Bind [Pattern] Term] -> Decl
  deriving Show

-- | Get the name that a declaration is about.
declName :: Decl -> Name Term
declName (DType x _) = x
declName (DDefn x _) = x

-- | Check whether a declaration is a definition.
isDefn :: Decl -> Bool
isDefn DDefn{} = True
isDefn _       = False

-- | Injections into a sum type (@inl@ or @inr@) have a "side" (@L@ or @R@).
data Side = L | R
  deriving (Show, Eq, Enum)

-- | Unary operators.
data UOp = Neg   -- ^ Arithmetic negation (@-@)
         | Not   -- ^ Logical negation (@not@)
         | Fact  -- ^ Factorial (@!@)
         | Sqrt  -- ^ Integer square root (@sqrt@)
         | Lg    -- ^ Floor of base-2 logarithm (@lg@)
         | Floor -- ^ Floor of fractional type (@floor@)
         | Ceil  -- ^ Ceiling of fractional type (@ceiling@)
  deriving (Show, Eq, Ord)

-- | Binary operators.
data BOp = Add     -- ^ Addition (@+@)
         | Sub     -- ^ Subtraction (@-@)
         | Mul     -- ^ Multiplication (@*@)
         | Div     -- ^ Division (@/@)
         | Exp     -- ^ Exponentiation (@^@)
         | IDiv    -- ^ Integer division (@//@)
         | Eq      -- ^ Equality test (@==@)
         | Neq     -- ^ Not-equal (@/=@)
         | Lt      -- ^ Less than (@<@)
         | Gt      -- ^ Greater than (@>@)
         | Leq     -- ^ Less than or equal (@<=@)
         | Geq     -- ^ Greater than or equal (@>=@)
         | And     -- ^ Logical and (@&&@ / @and@)
         | Or      -- ^ Logical or (@||@ / @or@)
         | Mod     -- ^ Modulo (@mod@)
         | Divides -- ^ Divisibility test (@|@)
         | RelPm   -- ^ Relative primality test (@#@)
         | Binom   -- ^ Binomial coefficient (@binom@)
         | Cons    -- ^ List cons (@::@)
  deriving (Show, Eq, Ord)

-- | Fixities of unary operators (either pre- or postfix).
data UFixity
  = Pre     -- ^ Unary prefix.
  | Post    -- ^ Unary postfix.
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Fixity of infix binary operators (either left, right, or non-associative).
data BFixity
  = InL   -- ^ Left-associative infix.
  | InR   -- ^ Right-associative infix.
  | In    -- ^ Infix.
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Operators together with their fixity.
data OpFixity =
    UOpF UFixity UOp
  | BOpF BFixity BOp
  deriving (Eq, Show)

-- | An @OpInfo@ record contains information about an operator, such
--   as the operator itself, its fixity, a list of concrete syntax
--   representations of the operator, and a numeric precedence level.
data OpInfo =
  OpInfo
  { opFixity :: OpFixity
  , opSyns   :: [String]
  , opPrec   :: Int
  }
  deriving Show

-- | The @opTable@ lists all the operators in the language, in order
--   of precedence (highest precedence first).  Operators in the same
--   list have the same precedence.  This table is used by both the
--   parser and the pretty-printer.
opTable :: [[OpInfo]]
opTable =
  assignPrecLevels $
  [ [ uopInfo Pre  Not     ["not", "¬"]
    ]
  , [ uopInfo Pre  Neg     ["-"]
    ]
  , [ uopInfo Post Fact    ["!"]
    ]
  , [ bopInfo InR  Exp     ["^"]
    ]
  , [ uopInfo Pre  Sqrt    ["sqrt"]
    ]
  , [ uopInfo Pre  Lg      ["lg"]
    ]
  , [ uopInfo Pre  Floor   ["floor"]
    , uopInfo Pre  Ceil    ["ceiling"]
    ]
  , [ bopInfo In   Binom   ["choose"]
    ]
  , [ bopInfo InL  Mul     ["*"]
    , bopInfo InL  Div     ["/"]
    , bopInfo InL  Mod     ["%"]
    , bopInfo InL  Mod     ["mod"]
    , bopInfo InL  IDiv    ["//"]
    ]
  , [ bopInfo InL  Add     ["+"]
    , bopInfo InL  Sub     ["-"]
    ]
  , [ bopInfo InR  Cons    ["::"]
    ]
  , [ bopInfo InR  Eq      ["="]
    , bopInfo InR  Neq     ["/="]
    , bopInfo InR  Lt      ["<"]
    , bopInfo InR  Gt      [">"]
    , bopInfo InR  Leq     ["<="]
    , bopInfo InR  Geq     [">="]
    , bopInfo InR  Divides ["divides"]
    , bopInfo InR  RelPm   ["#"]
    ]
  , [ bopInfo InR  And     ["and", "∧", "&&"]
    ]
  , [ bopInfo InR  Or      ["or", "∨", "||"]
    ]
  ]
  where
    uopInfo fx op syns = OpInfo (UOpF fx op) syns (-1)
    bopInfo fx op syns = OpInfo (BOpF fx op) syns (-1)

    assignPrecLevels table = zipWith assignPrecs (reverse [1 .. length table]) table
    assignPrecs p ops      = map (assignPrec p) ops
    assignPrec  p op       = op { opPrec = p }

-- | A map from all unary operators to their associated 'OpInfo' records.
uopMap :: M.Map UOp OpInfo
uopMap = M.fromList $
  [ (op, info) | opLevel <- opTable, info@(OpInfo (UOpF _ op) _ _) <- opLevel ]

-- | A map from all binary operators to their associatied 'OpInfo' records.
bopMap :: M.Map BOp OpInfo
bopMap = M.fromList $
  [ (op, info) | opLevel <- opTable, info@(OpInfo (BOpF _ op) _ _) <- opLevel ]

-- | Type Operators
data TyOp = Enumerate -- List all values of a type
          | Count     -- Count how many values there are of a type
  deriving (Show, Eq)

-- | Terms.
data Term where

  -- | A variable.
  TVar   :: Name Term -> Term

  -- | The unit value, (), of type Unit.
  TUnit  :: Term

  -- | True or false.
  TBool  :: Bool -> Term

  -- | An anonymous function.
  TAbs   :: Bind (Name Term) Term -> Term

     -- Note, could add an optional type annotation to TAbs,
     -- problem is I don't know what would be a good concrete syntax!
     -- x : Int -> body  is tricky because when parsing the type,
     -- the -> looks like a type arrow.  Could perhaps require
     -- parens i.e.  (x : Int) -> body ?

  -- | Juxtaposition (can be either function application or
  --   multiplication).
  TJuxt  :: Term -> Term -> Term

  -- | An ordered pair, @(x,y)@.
  TTup   :: [Term] -> Term

  -- | An injection into a sum type.
  TInj   :: Side -> Term -> Term

  -- | A natural number.
  TNat   :: Integer -> Term

  -- | A nonnegative rational number, parsed as a decimal.
  TRat   :: Rational -> Term

  -- | An application of a unary operator.
  TUn    :: UOp -> Term -> Term

  -- | An application of a binary operator.
  TBin   :: BOp -> Term -> Term -> Term

  -- | An application of a type operator.
  TTyOp  :: TyOp -> Type -> Term

  -- | A chained comparison.  Should contain only comparison
  --   operators.
  TChain :: Term -> [Link] -> Term


  -- | A literal list.
  TList :: [Term] -> Term

  -- | A (non-recursive) let expression, @let x = t1 in t2@.
  TLet   :: Bind (Name Term, Embed Term) Term -> Term

  -- | A case expression.
  TCase  :: [Branch] -> Term

  -- | Type ascription, @(term : type)@.
  TAscr  :: Term -> Type -> Term
  deriving Show

data Link where
  TLink :: BOp -> Term -> Link
  deriving Show

-- | A branch of a case is a list of guards with an accompanying term.
--   The guards scope over the term.  Additionally, each guard scopes
--   over subsequent guards.
type Branch = Bind Guards Term

-- | A list of guards.  Variables bound in each guard scope over
--   subsequent ones.
data Guards where

  -- | The empty list of guards, /i.e./ @otherwise@.
  GEmpty :: Guards

  -- | A single guard (@if@ or @when@) followed by more guards.
  GCons  :: Rebind Guard Guards -> Guards

  deriving Show

-- | A single guard in a branch: either an @if@ or a @when@.
data Guard where

  -- | Boolean guard (@if <test>@)
  GBool :: Embed Term -> Guard

  -- | Pattern guard (@when term = pat@)
  GPat  :: Embed Term -> Pattern -> Guard

  deriving Show

-- | Patterns.
data Pattern where

  -- | Variable pattern: matches anything and binds the variable.
  PVar  :: Name Term -> Pattern

  -- | Wildcard pattern @_@: matches anything.
  PWild :: Pattern

  -- | Unit pattern @()@: matches @()@.
  PUnit :: Pattern

  -- | Literal boolean pattern.
  PBool :: Bool -> Pattern

  -- | Tuple pattern @(pat1, .. , patn)@.
  PTup  :: [Pattern] -> Pattern

  -- | Injection pattern (@inl pat@ or @inr pat@).
  PInj  :: Side -> Pattern -> Pattern

  -- | Literal natural number pattern.
  PNat  :: Integer -> Pattern

  -- | Successor pattern, @S p@.
  PSucc :: Pattern -> Pattern

  -- | Cons pattern @p1 :: p2@.
  PCons :: Pattern -> Pattern -> Pattern

  -- | List pattern @[p1, .., pn]@.
  PList :: [Pattern] -> Pattern

  deriving Show
  -- TODO: figure out how to match on Z or Q!

derive [''Side, ''UOp, ''BOp, ''TyOp, ''Term, ''Link, ''Guards, ''Guard, ''Pattern]

instance Alpha Rational   -- XXX orphan!
instance Alpha Side
instance Alpha UOp
instance Alpha BOp
instance Alpha TyOp
instance Alpha Link
instance Alpha Term
instance Alpha Guards
instance Alpha Guard
instance Alpha Pattern

instance Subst Term Rational
instance Subst Term Type
instance Subst Term Guards
instance Subst Term Guard
instance Subst Term Pattern
instance Subst Term Side
instance Subst Term BOp
instance Subst Term UOp
instance Subst Term TyOp
instance Subst Term Link
instance Subst Term Term where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing
