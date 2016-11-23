{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Disco.AST.Surface where

import           Unbound.LocallyNameless

-- | A program is a list of declarations.
type Prog = [Decl]

-- | A declaration is either a type declaration or (one clause of a) definition.
data Decl where
  DType :: Name Term -> Type -> Decl
  DDefn :: Name Term -> Bind [Pattern] Term -> Decl
  deriving Show

-- | Injections into a sum type (inl or inr) have a "side" (L or R).
data Side = L | R
  deriving (Show, Eq, Enum)

-- | Unary operators.
data UOp = Neg | Not
  deriving (Show, Eq)

-- | Binary operators.
data BOp = Add | Sub | Mul | Div | Exp | Eq | Neq | Lt | Gt | Leq | Geq | And | Or | Mod
         | Divides | RelPm
  deriving (Show, Eq)

-- XXX todo add TRat with ability to parse decimal notation

-- | Terms.
data Term where
  TVar   :: Name Term -> Term                  -- ^ Variable
  TUnit  :: Term                               -- ^ Unit ()
  TBool  :: Bool -> Term                       -- ^ Boolean
  TAbs   :: Bind (Name Term) Term -> Term      -- ^ Anonymous function abstraction

     -- Note, could add an optional type annotation to TAbs,
     -- problem is I don't know what would be a good concrete syntax!
     -- x : Int -> body  is tricky because when parsing the type,
     -- the -> looks like a type arrow.  Could perhaps require
     -- parens i.e.  (x : Int) -> body ?
  TJuxt  :: Term -> Term -> Term               -- ^ Juxtaposition (can be either
                                               --   function application or multiplication)
  TPair  :: Term -> Term -> Term               -- ^ Ordered pairs (x,y)
  TInj   :: Side -> Term -> Term               -- ^ Injection into a sum type
  TNat   :: Integer -> Term                    -- ^ A natural number
  TUn    :: UOp -> Term -> Term                -- ^ Application of a unary operator
  TBin   :: BOp -> Term -> Term -> Term        -- ^ Application of a binary operator
  TLet   :: Bind (Name Term, Embed Term) Term -> Term
                                               -- ^ Non-recursive let expression
                                               --   (let x = t1 in t2)
  TCase  :: [Branch] -> Term                   -- ^ A case expression
                                               --   consists of a list
                                               --   of branches.
  TAscr  :: Term -> Type -> Term               -- ^ Type ascription (expr : type)
  deriving Show

-- | A branch of a case is a list of guards with an accompanying term.
--   The guards scope over the term.  Additionally, each guard scopes
--   over subsequent guards.
type Branch = Bind Guards Term

data Guards where
  GEmpty :: Guards
  GCons  :: Rebind Guard Guards -> Guards
  deriving Show

-- | A single guard in a branch.
data Guard where
  GIf   :: Embed Term -> Guard             -- ^ Boolean guard (if <test>)
  GWhen :: Embed Term -> Pattern -> Guard  -- ^ Pattern guard (when term = pat)
  deriving Show

-- | Patterns.
data Pattern where
  PVar  :: Name Term -> Pattern             -- ^ Variable
  PWild :: Pattern                          -- ^ Wildcard _
  PUnit :: Pattern                          -- ^ Unit ()
  PBool :: Bool -> Pattern                  -- ^ Literal boolean
  PPair :: Pattern -> Pattern -> Pattern    -- ^ Pair pattern (pat1, pat2)
  PInj  :: Side -> Pattern -> Pattern       -- ^ Injection pattern (inl pat or inr pat)
  PNat  :: Integer -> Pattern               -- ^ Literal natural number pattern
  PSucc :: Pattern -> Pattern               -- ^ Successor pattern, (succ n)
  deriving Show
  -- TODO: figure out how to match on Z or Q!

-- | Types.
data Type where
  TyVar    :: Name Type -> Type
    -- Unification variables.  Ideally Type would be parameterized by
    -- a variable type, then we could use Type' Void to represent
    -- solved types, but I can't figure out how to make that work with
    -- unbound.

  TyVoid   :: Type                  -- ^ Void
  TyUnit   :: Type                  -- ^ Unit
  TyBool   :: Type                  -- ^ Bool
  TyArr    :: Type -> Type -> Type  -- ^ Function type,  T1 -> T2
  TyPair   :: Type -> Type -> Type  -- ^ Pair type, T1 * T2
  TySum    :: Type -> Type -> Type  -- ^ Sum type, T1 + T2
  TyN      :: Type                  -- ^ Natural numbers
  TyZ      :: Type                  -- ^ Integers
  TyQ      :: Type                  -- ^ Rationals
  deriving (Show, Eq)

derive [''Side, ''UOp, ''BOp, ''Term, ''Guards, ''Guard, ''Pattern, ''Type]

instance Alpha Side
instance Alpha UOp
instance Alpha BOp
instance Alpha Term
instance Alpha Guards
instance Alpha Guard
instance Alpha Pattern
instance Alpha Type

instance Subst Term Type
instance Subst Term Guards
instance Subst Term Guard
instance Subst Term Pattern
instance Subst Term Side
instance Subst Term BOp
instance Subst Term UOp
instance Subst Term Term where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing

isNumTy :: Type -> Bool
isNumTy ty = ty `elem` [TyN, TyZ, TyQ]
