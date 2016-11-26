{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}

module Disco.Typecheck where

import           Prelude                 hiding (lookup)

import           Control.Applicative     ((<|>))
import           Control.Arrow           ((&&&))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List               (group, partition, sort)
import qualified Data.Map                as M

import           Unbound.LocallyNameless hiding (comp)

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Types

type Defn  = Bind [Pattern] ATerm
type Defns = M.Map (Name ATerm) Defn

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
  | EmptyCase              -- ^ Case analyses cannot be empty.
  | NoLub Type Type        -- ^ The given types have no lub.
  | PatternType Pattern Type  -- ^ The given pattern should have the type, but it doesn't.
  | ModQ                   -- ^ Can't do mod on rationals.
  | ExpQ                   -- ^ Can't exponentiate by a rational.
  | RelPmQ                 -- ^ Can't ask about relative primality of rationals.
  | DuplicateDecls (Name Term)  -- ^ Duplicate declarations.
  | DuplicateDefns (Name Term)  -- ^ Duplicate definitions.
  | NumPatterns            -- ^ # of patterns does not match type in definition
  | NotList Term Type      -- ^ Should have a list type, but expected to have some other type
  | NoError                -- ^ Not an error.  The identity of the
                           --   @Monoid TCError@ instance.
  deriving Show

-- | 'TCError' is a monoid where we simply discard the first error.
instance Monoid TCError where
  mempty = NoError
  mappend _ r = r

-- | TypeCheckingMonad. Maintains a context of variable types and a
--   set of definitions, and can throw @TCError@s and generate fresh
--   names.
type TCM = StateT Defns (ReaderT Ctx (ExceptT TCError LFreshM))

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError (a, Defns)
runTCM = runLFreshM . runExceptT . flip runReaderT emptyCtx . flip runStateT M.empty

evalTCM :: TCM a -> Either TCError a
evalTCM = fmap fst . runTCM

execTCM :: TCM a -> Either TCError Defns
execTCM = fmap snd . runTCM

emptyCtx :: Ctx
emptyCtx = M.empty

singleCtx :: Name Term -> Type -> Ctx
singleCtx = M.singleton

joinCtx :: Ctx -> Ctx -> Ctx
joinCtx = M.union

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

-- | Run a @TCM@ computation in a context extended with an additional
--   context.
extends :: Ctx -> TCM a -> TCM a
extends ctx = local (joinCtx ctx)

-- | Check that a term has the given type.  Either throws an error, or
--   returns the term annotated with types for all subterms.
check :: Term -> Type -> TCM ATerm

  -- We can check that the empty list has any list type.
check (TList []) ty@(TyList _) = return $ ATList ty []
check (TList []) ty            = throwError (NotList (TList []) ty)

  -- To check that an abstraction has an arrow type, check that the
  -- body has the return type under an extended context.
check (TAbs lam) ty@(TyArr ty1 ty2) = do
  lunbind lam $ \(x,t) -> do
  extend x ty1 $ do
  at <- check t ty2
  return $ ATAbs ty (bind (translate x) at)
  -- We are trying to check an abstraction under a non-arrow type: error.
check t@(TAbs _) ty = throwError (NotArrow t ty)

  -- To check an injection has a sum type, recursively check the
  -- relevant type.
check (TInj L t) ty@(TySum ty1 _) = do
  at <- check t ty1
  return $ ATInj ty L at
check (TInj R t) ty@(TySum _ ty2) = do
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
lub ty1 ty2 = throwError $ NoLub ty1 ty2

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
isDecidable (TyList ty) = isDecidable ty

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
isOrdered (TyList ty) = isOrdered ty

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

infer (TBin Exp t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  checkNumTy at1
  checkNumTy at2
  case getType at2 of
    TyN -> return $ ATBin (getType at1) Exp at1 at2
    TyZ -> return $ ATBin TyQ Exp at1 at2
    TyQ -> throwError ExpQ
    _   -> error "Impossible! getType at2 is not num type after checkNumTy"

  -- An equality test always has type Bool, but we need to check a few
  -- things first. We infer the types of both subterms and check that
  -- (1) they have a common supertype which (2) has decidable
  -- equality.
infer (TBin Eq t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  ty3 <- lub (getType at1) (getType at2)
  checkDecidable ty3
  return $ ATBin TyBool Eq at1 at2

infer (TBin op t1 t2)
  | op `elem` [Lt, Gt, Leq, Geq] = inferComp op t1 t2

  -- &&, ||, and not always have type Bool, and the subterms must have type
  -- Bool as well.
infer (TBin And t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool And at1 at2
infer (TBin Or t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool Or at1 at2
infer (TUn Not t) = do
  at <- check t TyBool
  return $ ATUn TyBool Not at

infer (TBin Mod t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  ty <- numLub at1 at2
  if (isSub ty TyZ)
    then return (ATBin ty Mod at1 at2)
    else throwError ModQ

infer (TBin Divides t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  _ <- numLub at1 at2
  return (ATBin TyBool Divides at1 at2)

infer (TBin RelPm t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  ty <- numLub at1 at2
  if (isSub ty TyZ)
    then return (ATBin TyBool RelPm at1 at2)
    else throwError RelPmQ

infer (TBin Binom t1 t2) = do
  at1 <- check t1 TyN     -- XXX for now.  Can handle negative or even Q arguments,
  at2 <- check t2 TyN     -- but should we?
  return $ ATBin TyN Binom at1 at2

infer (TBin Cons t1 t2) = do
  at1 <- infer t1
  at2 <- check t2 (TyList (getType at1))
  return $ ATBin (TyList (getType at1)) Cons at1 at2

infer (TUn Fact t) = do
  at <- check t TyN
  return $ ATUn TyN Fact at

infer (TList (e:es)) = do
  ate  <- infer e
  let ty = getType ate
  ates <- mapM (flip check ty) es
  return $ ATList (TyList ty) (ate : ates)

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

infer (TCase []) = throwError EmptyCase
infer (TCase bs) = inferCase bs

  -- Catch-all case at the end: if we made it here, we can't infer it.
infer t = throwError (CantInfer t)


inferFunApp :: ATerm -> Term -> TCM ATerm
inferFunApp at t' = do
  (ty1, ty2) <- getFunTy at
  at' <- check t' ty1
  return $ ATApp ty2 at at'

inferMulApp :: ATerm -> Term -> TCM ATerm
inferMulApp at t' = do
  at' <- infer t'
  num3 <- numLub at at'
  return $ ATBin num3 Mul at at'

  -- A comparison always has type Bool, but we have to make sure
  -- the subterms are OK. We must check that their types are
  -- compatible and have a total order.
inferComp :: BOp -> Term -> Term -> TCM ATerm
inferComp comp t1 t2 = do
  at1 <- infer t1
  at2 <- infer t2
  ty3 <- lub (getType at1) (getType at2)
  checkOrdered ty3
  return $ ATBin TyBool comp at1 at2

inferCase :: [Branch] -> TCM ATerm
inferCase bs = do
  bs' <- mapM inferBranch bs
  let (branchTys, abs') = unzip bs'
  resTy <- foldM lub TyVoid branchTys
  return $ ATCase resTy abs'

inferBranch :: Branch -> TCM (Type, ABranch)
inferBranch b =
  lunbind b $ \(gs, t) -> do
  (ags, ctx) <- inferGuards gs
  extends ctx $ do
  at <- infer t
  return $ (getType at, bind ags at)

inferGuards :: Guards -> TCM (AGuards, Ctx)
inferGuards GEmpty                       = return (AGEmpty, emptyCtx)
inferGuards (GCons (unrebind -> (g,gs))) = do
  (ag, ctx) <- inferGuard g
  extends ctx $ do
  (ags, ctx') <- inferGuards gs
  return (AGCons (rebind ag ags), ctx `joinCtx` ctx')

inferGuard :: Guard -> TCM (AGuard, Ctx)
inferGuard (GIf (unembed -> t)) = do
  at <- check t TyBool
  return (AGIf (embed at), emptyCtx)
inferGuard (GWhen (unembed -> t) p) = do
  at <- infer t
  ctx <- checkPattern p (getType at)
  return (AGWhen (embed at) p, ctx)

-- XXX todo: check for nonlinear patterns.
-- Nonlinear patterns can desugar to equality checks!
-- Currently  { x when (3,4) = (x,x)   evaluates without error to 3.
-- It should be accepted, but fail to match since 3 != 4.
checkPattern :: Pattern -> Type -> TCM Ctx
checkPattern (PVar x) ty                    = return $ singleCtx x ty
checkPattern PWild    _                     = ok
checkPattern PUnit TyUnit                   = ok
checkPattern PEmpty (TyList _)              = ok
checkPattern (PCons p1 p2) (TyList ty)      =
  joinCtx <$> checkPattern p1 ty <*> checkPattern p2 (TyList ty)
checkPattern (PBool _) TyBool               = ok
checkPattern (PPair p1 p2) (TyPair ty1 ty2) =
  joinCtx <$> checkPattern p1 ty1 <*> checkPattern p2 ty2
checkPattern (PInj L p) (TySum ty1 _)       = checkPattern p ty1
checkPattern (PInj R p) (TySum _ ty2)       = checkPattern p ty2
checkPattern (PNat _)   ty | isSub TyN ty   = ok
  -- we can match any supertype of TyN against a Nat pattern
checkPattern (PSucc p)  TyN                 = checkPattern p TyN

checkPattern p ty = throwError (PatternType p ty)

ok :: TCM Ctx
ok = return emptyCtx

------------------------------------------------------------

addDefn :: Name Term -> (Bind [Pattern] ATerm) -> TCM ()
addDefn x b = modify (M.insert (translate x) b)

declName :: Decl -> Name Term
declName (DType x _) = x
declName (DDefn x _) = x

inferProg :: Prog -> TCM Ctx
inferProg prog = do
  let (defns, typeDecls) = partition isDefn prog
  withTypeDecls typeDecls $ do
    mapM_ checkDefn defns
    ask

-- precondition: only called on DTypes
withTypeDecls :: [Decl] -> TCM a -> TCM a
withTypeDecls decls k = do
  let dups :: [(Name Term, Int)]
      dups = filter ((>1) . snd) . map (head &&& length) . group . sort . map declName $ decls
  case dups of
    ((x,_):_) -> throwError (DuplicateDecls x)
    []        -> extends declCtx k
  where
    declCtx = M.fromList (map (\(DType x ty) -> (x,ty)) decls)

-- precondition: only called on DDefns
checkDefn :: Decl -> TCM ()
checkDefn (DDefn x def) = do
  ty <- lookup x
  prevDefn <- gets (M.lookup (translate x))
  case prevDefn of
    Just _ -> throwError (DuplicateDefns x)
    Nothing -> lunbind def $ \(pats, body) -> do
      at <- go pats ty body
      addDefn x (bind pats at)
  where
    go [] ty body = check body ty
    go (p:ps) (TyArr ty1 ty2) body = do
      ctx <- checkPattern p ty1
      extends ctx $ go ps ty2 body
    go _ _ _ = throwError NumPatterns   -- XXX include more info
checkDefn d = error $ "Impossible! checkDefn called on non-Defn: " ++ show d
