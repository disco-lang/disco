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

data Expr where
  Const :: Int -> Expr
  Add   :: Expr -> Expr -> Expr

data AST :: * -> * where
  Expr :: AST Expr

deriveAll ''AST
