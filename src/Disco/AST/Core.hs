{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Disco.AST.Core where

import           Unbound.LocallyNameless

import           Disco.Types

-- | Core desugared language.  Mostly untyped (i.e. types have been
--   erased).
data Core where
  CVar  :: Name Core -> Core              -- ^ A variable.
  CCons :: Int -> [Core] -> Core          -- ^ A constructor, identified by number,
                                          --   plus arguments.  Note we do not need
                                          --   to remember which type the constructor
                                          --   came from; if the program typechecked
                                          --   then we will never compare constructors
                                          --   from different types.
  CNat  :: Integer -> Core                -- ^ A natural number.
  CAbs  :: Bind (Name Core) Core -> Core  -- ^ Lambda abstraction.
  CApp  :: Strictness -> Core -> Core -> Core   -- ^ Function application, with strictness.
  COp   :: Op -> [Core] -> Core                 -- ^ Operator application.
  CLet  :: Strictness -> Bind (Name Core, Embed Core) Core -> Core
                                          -- ^ Non-recursive let, with strictness.
  CCase :: [CBranch] -> Core              -- ^ Case expression.
  deriving Show

-- | Operators that can show up in the core language.  Note that not
--   all surface language operators show up here, since some can be
--   desugared into combinators of the operators here.
data Op = OAdd | ONeg | OMul | ODiv | OExp | OAnd | OOr | OMod | ODivides | ORelPm
        | OEq Type | OLt Type | ONot
  deriving Show

type CBranch = Bind CGuards Core

data CGuards where
  CGEmpty :: CGuards
  CGCons  :: Rebind (Embed Core, CPattern) CGuards -> CGuards
  deriving Show

-- | Core (desugared) pattern.  We only need variables, wildcards,
--   nats, and constructors.
data CPattern where
  CPVar  :: Name Core -> CPattern
  CPWild :: CPattern
  CPCons :: Int -> [CPattern] -> CPattern
  CPNat  :: Integer -> CPattern
  CPSucc :: CPattern -> CPattern
  deriving Show

derive [''Strictness, ''Core, ''Op, ''CPattern, ''CGuards]

instance Alpha Strictness
instance Alpha Core
instance Alpha Op
instance Alpha CPattern
instance Alpha CGuards

