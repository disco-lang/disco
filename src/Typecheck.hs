{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}


module Typecheck where

import Prelude hiding (lookup)

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                as M

import           Unbound.LocallyNameless

import           Types

-- Terms with type annotations.
-- Should probably really do this with a 2-level/open recursion
-- approach, with a free comonad or whatever

data ATerm where
  ATVar   :: Type -> Name Term -> ATerm
  ATUnit  :: ATerm
  ATBool  :: Bool -> ATerm
  ATAbs   :: Type -> Bind (Name ATerm) ATerm -> ATerm
  ATApp   :: Type -> ATerm -> ATerm -> ATerm
  ATPair  :: Type -> ATerm -> ATerm -> ATerm
  ATInj   :: Type -> Side -> ATerm -> ATerm
  ATInt   :: Integer -> ATerm
  ATUn    :: Type -> UOp -> ATerm -> ATerm
  ATBin   :: Type -> BOp -> ATerm -> ATerm -> ATerm
  ATLet   :: Type -> Bind (Rec (Name ATerm, Embed ATerm)) ATerm -> ATerm
  ATCase  :: Type -> [Branch] -> ATerm
  ATWrong :: Type -> ATerm
  ATSub   :: Type -> ATerm -> ATerm
  deriving Show

derive [''ATerm]

instance Alpha ATerm

getType :: ATerm -> Type
getType (ATVar ty _)     = ty
getType ATUnit           = TyUnit
getType (ATBool _)       = TyBool
getType (ATAbs ty _)     = ty
getType (ATApp ty _ _)   = ty
getType (ATPair ty _ _)  = ty
getType (ATInj ty _ _)   = ty
getType (ATInt _)        = TyN      -- XXX something fishy here with numerics
getType (ATUn ty _ _)    = ty
getType (ATBin ty _ _ _) = ty
getType (ATLet ty _)     = ty
getType (ATCase ty _)    = ty
getType (ATWrong ty)     = ty

type Ctx = M.Map (Name Term) Type

data TCError
  = Unbound (Name Term)
  | NotArrow Term Type
  | NotSum Term Type
  | Mismatch Term Type Type    -- term, expected, actual
  | CantInfer Term

type TCM = ReaderT Ctx (ExceptT TCError LFreshM)

lookup :: Name Term -> TCM Type
lookup x = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing -> throwError (Unbound x)
    Just ty -> return ty

extend :: Name Term -> Type -> TCM a -> TCM a
extend x ty = local (M.insert x ty)

check :: Term -> Type -> TCM ATerm

check (TAbs abs) ty@(TyArr ty1 ty2) = do
  lunbind abs $ \(x,t) -> do
  extend x ty1 $ do
  at <- check t ty2
  return $ ATAbs ty (bind (translate x) at)
check t@(TAbs _) ty = throwError (NotArrow t ty)

check (TInj L t) ty@(TySum ty1 ty2) = do
  at <- check t ty1
  return $ ATInj ty L at
check (TInj R t) ty@(TySum ty1 ty2) = do
  at <- check t ty2
  return $ ATInj ty R at
check t@(TInj _ _) ty = throwError (NotSum t ty)

check t ty = do
  at <- infer t
  let ty' = getType at
  checkSub at ty

checkSub :: ATerm -> Type -> TCM ATerm
checkSub at ty = do
  let ty' = getType at
  if (ty' == ty)
     then return at
     else return (ATSub ty at)

isSub :: Type -> Type -> Bool
isSub ty1 ty2 | ty1 == ty2 = True
isSub TyN TyZ = True
isSub TyN TyQ = True
isSub TyZ TyQ = True
isSub (TyPair t1 t2) (TyPair t1' t2') = isSub t1 t1' && isSub t2 t2'
isSub (TySum  t1 t2) (TySum  t1' t2') = isSub t1 t1' && isSub t2 t2'

infer :: Term -> TCM ATerm
infer (TVar x)      = do
  ty <- lookup x
  return $ ATVar ty x
infer TUnit         = return ATUnit
infer (TBool b)     = return $ ATBool b
infer (TApp t t')   = undefined   -- XXX todo: app
infer (TPair t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  return $ ATPair (TyPair (getType at1) (getType at2)) at1 at2
infer (TInt n)      = return $ ATInt n

-- XXX todo: arithmetic operators, with lub etc.

infer (TLet l) = do
  lunbind l $ \(unrec -> (x, unembed -> t1), t2) -> do
  at1 <- infer t1
  let ty1 = getType at1
  extend x ty1 $ do
  at2 <- infer t2
  return $ ATLet (getType at2) (bind (rec (translate x, embed at1)) at2)

-- XXX todo: case

infer t = throwError (CantInfer t)
