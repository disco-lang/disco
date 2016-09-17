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

import Control.Applicative ((<|>))
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
  ATAscr  :: ATerm -> Type -> ATerm
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
getType (ATAscr _ ty)    = ty
getType (ATSub ty _)     = ty

type Ctx = M.Map (Name Term) Type

data TCError
  = Unbound (Name Term)
  | NotArrow Term Type     -- the type of a lambda should be an arrow type but isn't
  | NotFun   ATerm         -- the term should be a function but has a non-arrow type
  | NotSum Term Type
  | Mismatch Type ATerm    -- expected, actual
  | CantInfer Term
  | NotNum ATerm
  | IncompatibleTypes Type Type
  | Application ATerm Term  -- trying to apply fst to snd, not a function or not numeric etc.
  | NoError
  deriving Show

instance Monoid TCError where
  mempty = NoError
  mappend _ r = r

type TCM = ReaderT Ctx (ExceptT TCError LFreshM)

runTCM :: TCM a -> Either TCError a
runTCM = runLFreshM . runExceptT . flip runReaderT M.empty

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
  checkSub at ty

checkSub :: ATerm -> Type -> TCM ATerm
checkSub at ty = do
  let ty' = getType at
  if (isSub ty' ty)
   then
     if (ty' == ty)
       then return at
       else return (ATSub ty at)
   else
     throwError (Mismatch ty at)

isSub :: Type -> Type -> Bool
isSub ty1 ty2 | ty1 == ty2 = True
isSub TyN TyZ = True
isSub TyN TyQ = True
isSub TyZ TyQ = True
isSub (TyArr t1 t2) (TyArr t1' t2')   = isSub t1' t1 && isSub t2 t2'
isSub (TyPair t1 t2) (TyPair t1' t2') = isSub t1 t1' && isSub t2 t2'
isSub (TySum  t1 t2) (TySum  t1' t2') = isSub t1 t1' && isSub t2 t2'
isSub _ _ = False

lub :: Type -> Type -> TCM Type
lub ty1 ty2
  | isSub ty1 ty2 = return ty2
  | isSub ty2 ty1 = return ty1
lub (TyArr t1 t2) (TyArr t1' t2') = do
  requireSameTy t1 t1'
  t2'' <- lub t2 t2'
  return $ TyArr t1 t2''
lub (TyPair t1 t2) (TyPair t1' t2') = do
  t1'' <- lub t1 t1'
  t2'' <- lub t2 t2'
  return $ TyPair t1'' t2''
lub (TySum t1 t2) (TySum t1' t2') = do
  t1'' <- lub t1 t1'
  t2'' <- lub t2 t2'
  return $ TySum t1'' t2''

numLub :: ATerm -> ATerm -> TCM Type
numLub at1 at2 = do
  checkNumTy at1
  checkNumTy at2
  lub (getType at1) (getType at2)

requireSameTy :: Type -> Type -> TCM ()
requireSameTy ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise  = throwError $ IncompatibleTypes ty1 ty2

getFunTy :: ATerm -> TCM (Type, Type)
getFunTy (getType -> TyArr ty1 ty2) = return (ty1, ty2)
getFunTy at = throwError (NotFun at)

checkNumTy :: ATerm -> TCM ()
checkNumTy at =
  if (getType at `elem` [TyN, TyZ, TyQ])
     then return ()
     else throwError (NotNum at)

infer :: Term -> TCM ATerm
infer (TVar x)      = do
  ty <- lookup x
  return $ ATVar ty x
infer TUnit         = return ATUnit
infer (TBool b)     = return $ ATBool b
infer (TApp t t')   = do
  at <- infer t
  inferFunApp at t' <|> inferMulApp at t' <|> throwError (Application at t')
infer (TPair t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  return $ ATPair (TyPair (getType at1) (getType at2)) at1 at2
infer (TInt n)      = return $ ATInt n
infer (TBin Add t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  return $ ATBin num3 Add at1 at2
infer (TBin Mul t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  return $ ATBin num3 Mul at1 at2
infer (TBin Sub t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  num4 <- lub num3 TyZ
  return $ ATBin num4 Sub at1 at2
infer (TUn Neg t) = do
  at <- infer t
  checkNumTy at
  num2 <- lub (getType at) TyZ
  return $ ATUn num2 Neg at
infer (TBin Div t1 t2) = do
  at1 <- check t1 TyQ
  at2 <- check t2 TyQ
  return $ ATBin TyQ Div at1 at2
infer (TBin Equals t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  _ <- lub (getType at1) (getType at2)
  return $ ATBin TyBool Equals at1 at2
infer (TBin Less t1 t2) = do
  at1 <- check t1 TyQ
  at2 <- check t2 TyQ
  return $ ATBin TyBool Less at1 at2
infer (TBin And t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool And at1 at2
infer (TBin Or t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool Or at1 at2
infer (TLet l) = do
  lunbind l $ \(unrec -> (x, unembed -> t1), t2) -> do
  at1 <- infer t1
  let ty1 = getType at1
  extend x ty1 $ do
  at2 <- infer t2
  return $ ATLet (getType at2) (bind (rec (translate x, embed at1)) at2)
infer (TAscr t ty) = do
  at <- check t ty
  return $ ATAscr at ty

-- XXX todo: case

infer t = throwError (CantInfer t)


inferFunApp at t' = do
  (ty1, ty2) <- getFunTy at
  at' <- check t' ty1
  return $ ATApp ty2 at at'

inferMulApp at t' = do
  at' <- infer t'
  num3 <- numLub at at'
  return $ ATBin num3 Mul at at'

------------------------------------------------------------

