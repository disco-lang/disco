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

-- TODO: Should probably really do this with a 2-level/open recursion
-- approach, with a cofree comonad or whatever

-- | An @ATerm@ is a typechecked term where every node in the tree has
--   been annotated with the type of the subterm rooted at that node.
data ATerm where
  ATVar   :: Type -> Name ATerm -> ATerm                -- ^ Variable with its type.
  ATUnit  :: ATerm                                      -- ^ Unit.  We don't bother
                                                        --   storing TyUnit here.
  ATBool  :: Bool -> ATerm                              -- ^ Bool.  Don't bother storing
                                                        --   the type.
  ATAbs   :: Type -> Bind (Name ATerm) ATerm -> ATerm   -- ^ Abstraction.
  ATApp   :: Type -> ATerm -> ATerm -> ATerm            -- ^ Application.
  ATPair  :: Type -> ATerm -> ATerm -> ATerm            -- ^ Pair.
  ATInj   :: Type -> Side -> ATerm -> ATerm             -- ^ Injection.
  ATNat   :: Integer -> ATerm                           -- ^ Natural number.
  ATUn    :: Type -> UOp -> ATerm -> ATerm              -- ^ Unary operator.
  ATBin   :: Type -> BOp -> ATerm -> ATerm -> ATerm     -- ^ Binary operator.
  ATLet   :: Type -> Bind (Name ATerm, Embed ATerm) ATerm -> ATerm  -- ^ (Non-recursive) let.
  ATCase  :: Type -> [Branch] -> ATerm                  -- ^ Case expression.
  ATWrong :: Type -> ATerm                              -- ^ Wrong can have any type.
  ATAscr  :: ATerm -> Type -> ATerm                     -- ^ Ascription.
  ATSub   :: Type -> ATerm -> ATerm                     -- ^ @ATSub@ is used to record
                                                        --   the fact that we made use of
                                                        --   a subtyping judgment.
                                                        --   The term has the given type T
                                                        --   because its type is a subtype
                                                        --   of T.
  deriving Show
  -- TODO: I don't think we are currently very consistent about using ATSub everywhere
  --   subtyping is invoked.  I am not sure how much it matters.

derive [''ATerm]

instance Alpha ATerm

-- | Get the type at the root of an 'ATerm'.
getType :: ATerm -> Type
getType (ATVar ty _)     = ty
getType ATUnit           = TyUnit
getType (ATBool _)       = TyBool
getType (ATAbs ty _)     = ty
getType (ATApp ty _ _)   = ty
getType (ATPair ty _ _)  = ty
getType (ATInj ty _ _)   = ty
getType (ATNat _)        = TyN
getType (ATUn ty _ _)    = ty
getType (ATBin ty _ _ _) = ty
getType (ATLet ty _)     = ty
getType (ATCase ty _)    = ty
getType (ATWrong ty)     = ty
getType (ATAscr _ ty)    = ty
getType (ATSub ty _)     = ty

type Ctx = M.Map (Name Term) Type

-- | Potential typechecking errors.
data TCError
  = Unbound (Name Term)    -- ^ Encountered an unbound variable
  | NotArrow Term Type     -- ^ The type of a lambda should be an arrow type but isn't
  | NotFun   ATerm         -- ^ The term should be a function but has a non-arrow type
  | NotSum Term Type       -- ^ The term is an injection but is
                           --   expected to have some type other than
                           --   a sum type
  | Mismatch Type ATerm    -- ^ Simple type mismatch: expected, actual
  | CantInfer Term         -- ^ We were asked to infer the type of the
                           --   term, but its type cannot be inferred
  | NotNum ATerm           -- ^ The term is expected to have a numeric type but it doesn't
  | IncompatibleTypes Type Type  -- ^ The types should have a lub
                                 -- (i.e. common supertype) but they
                                 -- don't.
  | Juxtaposition ATerm Term
                           -- ^ The first term is juxtaposed with the
                           --   second, but typechecks as neither
                           --   function application nor
                           --   multiplication
  | Undecidable Type       -- ^ The type should be decidable so we can
                           --   check equality, but it isn't.
  | Unordered Type         -- ^ The type should be totally ordered so
                           --   we can check less than, but it isn't.
  | NoError                -- ^ Not an error.  The identity of the
                           --   @Monoid TCError@ instance.
  deriving Show

-- | 'TCError' is a monoid where we simply discard the first error.
instance Monoid TCError where
  mempty = NoError
  mappend _ r = r

-- | TypeCheckingMonad. Maintains a context of variable types, and can
-- throw @TCError@s and generate fresh names.
type TCM = ReaderT Ctx (ExceptT TCError LFreshM)

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError a
runTCM = runLFreshM . runExceptT . flip runReaderT M.empty

-- | Look up a name in the context, either returning its type or
--   throwing an unbound variable exception if it is not found.
lookup :: Name Term -> TCM Type
lookup x = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing -> throwError (Unbound x)
    Just ty -> return ty

-- | Run a @TCM@ computation in a context extended with a new binding.
extend :: Name Term -> Type -> TCM a -> TCM a
extend x ty = local (M.insert x ty)

-- | Check that a term has the given type.  Either throws an error, or
--   returns the term annotated with types for all subterms.
check :: Term -> Type -> TCM ATerm

  -- To check that an abstraction has an arrow type, check that the
  -- body has the return type under an extended context.
check (TAbs abs) ty@(TyArr ty1 ty2) = do
  lunbind abs $ \(x,t) -> do
  extend x ty1 $ do
  at <- check t ty2
  return $ ATAbs ty (bind (translate x) at)
  -- We are trying to check an abstraction under a non-arrow type: error.
check t@(TAbs _) ty = throwError (NotArrow t ty)

  -- To check an injection has a sum type, recursively check the
  -- relevant type.
check (TInj L t) ty@(TySum ty1 ty2) = do
  at <- check t ty1
  return $ ATInj ty L at
check (TInj R t) ty@(TySum ty1 ty2) = do
  at <- check t ty2
  return $ ATInj ty R at
  -- Trying to check an injection under a non-sum type: error.
check t@(TInj _ _) ty = throwError (NotSum t ty)

  -- Finally, to check anything else, we can infer its type and then
  -- check that the inferred type is a subtype of the given type.
check t ty = do
  at <- infer t
  checkSub at ty

-- | Check that the given annotated term has a type which is a subtype
--   of the given type.  The returned annotated term may be the same
--   as the input, or it may be wrapped in 'ATSub' if we made a
--   nontrivial use of subtyping.
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

-- | Check whether one type is a subtype of another (we have decidable
--   subtyping).
isSub :: Type -> Type -> Bool
isSub ty1 ty2 | ty1 == ty2 = True
isSub TyVoid _ = True
isSub TyN TyZ = True
isSub TyN TyQ = True
isSub TyZ TyQ = True
isSub (TyArr t1 t2) (TyArr t1' t2')   = isSub t1' t1 && isSub t2 t2'
isSub (TyPair t1 t2) (TyPair t1' t2') = isSub t1 t1' && isSub t2 t2'
isSub (TySum  t1 t2) (TySum  t1' t2') = isSub t1 t1' && isSub t2 t2'
isSub _ _ = False

-- | Compute the least upper bound (least common supertype) of two
--   types.  Return the LUB, or throw an error if there isn't one.
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

-- | Convenience function that ensures the given annotated terms have
--   numeric types, AND computes their LUB.
numLub :: ATerm -> ATerm -> TCM Type
numLub at1 at2 = do
  checkNumTy at1
  checkNumTy at2
  lub (getType at1) (getType at2)

-- | Decide whether a type is finite.
isFinite :: Type -> Bool
isFinite TyVoid = True
isFinite TyUnit = True
isFinite TyBool = True
isFinite (TyPair ty1 ty2) = isFinite ty1 && isFinite ty2
isFinite (TySum ty1 ty2)  = isFinite ty1 && isFinite ty2
isFinite (TyArr ty1 ty2)  = isFinite ty1 && isFinite ty2
isFinite _ = False

-- | Decide whether a type has decidable equality.
isDecidable :: Type -> Bool
isDecidable TyVoid = True
isDecidable TyUnit = True
isDecidable TyBool = True
isDecidable TyN    = True
isDecidable TyZ    = True
isDecidable TyQ    = True
isDecidable (TyPair ty1 ty2) = isDecidable ty1 && isDecidable ty2
isDecidable (TySum  ty1 ty2) = isDecidable ty1 && isDecidable ty2
isDecidable (TyArr  ty1 ty2) = isFinite    ty1 && isDecidable ty2

-- | Check whether the given type has decidable equality, and throw an
--   error if not.
checkDecidable :: Type -> TCM ()
checkDecidable ty
  | isDecidable ty = return ()
  | otherwise      = throwError $ Undecidable ty

-- | Check whether the given type has a total order.
isOrdered :: Type -> Bool
isOrdered TyVoid = True
isOrdered TyUnit = True
isOrdered TyBool = True
isOrdered TyN    = True
isOrdered TyZ    = True
isOrdered TyQ    = True
isOrdered (TyPair ty1 ty2) = isOrdered ty1 && isOrdered ty2
isOrdered (TySum  ty1 ty2) = isOrdered ty1 && isOrdered ty2
isOrdered (TyArr  ty1 ty2) = isFinite ty1 && isOrdered ty1 && isOrdered ty2

-- | Check whether the given type has a total order, and throw an
--   error if not.
checkOrdered :: Type -> TCM ()
checkOrdered ty
  | isOrdered ty = return ()
  | otherwise    = throwError $ Unordered ty

-- | Require two types to be equal.
requireSameTy :: Type -> Type -> TCM ()
requireSameTy ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise  = throwError $ IncompatibleTypes ty1 ty2

-- | Require a term to have a function type, returning the decomposed
--   type if it does, throwing an error if not.
getFunTy :: ATerm -> TCM (Type, Type)
getFunTy (getType -> TyArr ty1 ty2) = return (ty1, ty2)
getFunTy at = throwError (NotFun at)

-- | Check that an annotated term has a numeric type.  Throw an error
--   if not.
checkNumTy :: ATerm -> TCM ()
checkNumTy at =
  if (getType at `elem` [TyN, TyZ, TyQ])
     then return ()
     else throwError (NotNum at)

-- | Infer the type of a term.  If it succeeds, it returns the term
--   with all subterms annotated.
infer :: Term -> TCM ATerm

  -- To infer the type of a variable, just look it up in the context.
infer (TVar x)      = do
  ty <- lookup x
  return $ ATVar ty (translate x)

  -- A few trivial cases.
infer TUnit         = return ATUnit
infer (TBool b)     = return $ ATBool b
infer (TNat n)      = return $ ATNat n

infer (TJuxt t t')   = do
  at <- infer t
  inferFunApp at t' <|> inferMulApp at t' <|> throwError (Juxtaposition at t')

  -- To infer the type of a pair, just infer the types of both components.
infer (TPair t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  return $ ATPair (TyPair (getType at1) (getType at2)) at1 at2

  -- To infer the type of addition or multiplication, infer the types
  -- of the subterms, check that they are numeric, and return their
  -- lub.
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

  -- Subtraction is similar, except that we must also lub with Z (a
  -- Nat minus a Nat is not a Nat, it is an Int).
infer (TBin Sub t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  num4 <- lub num3 TyZ
  return $ ATBin num4 Sub at1 at2

  -- Negation is similar to subtraction.
infer (TUn Neg t) = do
  at <- infer t
  checkNumTy at
  num2 <- lub (getType at) TyZ
  return $ ATUn num2 Neg at

  -- Division always has type Q.  We just have to check that
  -- both subterms can be given type Q.
infer (TBin Div t1 t2) = do
  at1 <- check t1 TyQ
  at2 <- check t2 TyQ
  return $ ATBin TyQ Div at1 at2

  -- An equality test always has type Bool, but we need to check a few
  -- things first. We infer the types of both subterms and check that
  -- (1) they have a common supertype which (2) has decidable
  -- equality.
infer (TBin Equals t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  ty3 <- lub (getType at1) (getType at2)
  checkDecidable ty3
  return $ ATBin TyBool Equals at1 at2

  -- A less-than test always has type Bool, but we have to make sure
  -- the subterms are OK. We must check that their types are
  -- compatible and have a total order.
infer (TBin Less t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  ty3 <- lub (getType at1) (getType at2)
  checkOrdered ty3
  return $ ATBin TyBool Less at1 at2

  -- && and || always have type Bool, and the subterms must have type
  -- Bool as well.
infer (TBin And t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool And at1 at2
infer (TBin Or t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool Or at1 at2

  -- TODO: this is actually wrong, it is not recursive at all.  I
  -- don't think we can actually handle letrec in this simple
  -- bidirectional system.  We need more high-powered type inference.

  -- To infer the type of (let x = t1 in t2), assuming it is
  -- NON-RECURSIVE, infer the type of t1, and then infer the type of
  -- t2 in an extended context.
infer (TLet l) = do
  lunbind l $ \((x, unembed -> t1), t2) -> do
  at1 <- infer t1
  let ty1 = getType at1
  extend x ty1 $ do
  at2 <- infer t2
  return $ ATLet (getType at2) (bind (translate x, embed at1) at2)

  -- Ascriptions are what let us flip from inference mode into
  -- checking mode.
infer (TAscr t ty) = do
  at <- check t ty
  return $ ATAscr at ty

-- XXX todo: case

  -- Catch-all case at the end: if we made it here, we can't infer it.
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

