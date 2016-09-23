{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Types where

import           Unbound.LocallyNameless

-- | A program is a list of declarations.
type Prog = [Decl]

-- | A declaration is either a type declaration or a definition.
data Decl where
  DType :: Name Term -> Type -> Decl
  DDefn :: Name Term -> Term -> Decl
  deriving Show

-- | Injections into a sum type (inl or inr) have a "side" (L or R).
data Side = L | R
  deriving (Show, Eq)

-- | Unary operators.
data UOp = Neg
  deriving (Show, Eq)

-- | Binary operators.
data BOp = Add | Sub | Mul | Div | Equals | Less | And | Or
  deriving (Show, Eq)

-- | Terms.
data Term where
  TVar   :: Name Term -> Term                  -- ^ Variable
  TUnit  :: Term                               -- ^ Unit ()
  TBool  :: Bool -> Term                       -- ^ Boolean
  TAbs   :: Bind (Name Term) Term -> Term      -- ^ Anonymous function abstraction
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
--   The guards scope over the term.
type Branch = Bind [Guard] Term

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

derive [''Side, ''UOp, ''BOp, ''Term, ''Guard, ''Pattern, ''Type]

instance Alpha Side
instance Alpha UOp
instance Alpha BOp
instance Alpha Term
instance Alpha Guard
instance Alpha Pattern
instance Alpha Type
