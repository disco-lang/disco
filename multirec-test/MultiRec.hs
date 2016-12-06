{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import           Generics.MultiRec
import           Generics.MultiRec.TH

type Var = String

data Expr where
  Const :: Int -> Expr
  Add   :: Expr -> Expr -> Expr
  Mul   :: Expr -> Expr -> Expr
  EVar  :: Var -> Expr
  Let   :: Decl -> Expr -> Expr

data Decl where
  (:=) :: Var -> Expr -> Decl
  Seq  :: Decl -> Decl -> Decl

data AST :: * -> * where
  Var  :: AST Var
  Expr :: AST Expr
  Decl :: AST Decl

deriveAll ''AST

main :: IO ()
main = putStrLn "Hello world!"

{-

/home/brent/projects/disco/test/MultiRec.hs:1:1: error:
    Exception when trying to run compile-time code:
      Prelude.foldr1: empty list
    Code: deriveAll ''AST

-}
