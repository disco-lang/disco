{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
  -- For MonadException instances, see below.

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Disco values and evaluation monad. This is the main monad used in
-- the REPL and for evaluation and pretty-printing.
--
-----------------------------------------------------------------------------

module Disco.Eval
       (

         -- * Values

         Value(.., VFun, VDelay)

         -- * Environments

       , Env, extendEnv, extendsEnv, getEnv, withEnv, withTopEnv
       , garbageCollect

         -- * Memory cells

       , Cell(..), mkCell

         -- * Errors

       , IErr(..)

         -- * Disco monad state

       , Loc, DiscoState(..), initDiscoState

         -- ** Lenses

       , topCtx, topDefns, topTyDefns, topDocs, topEnv
       , memory, nextLoc, lastFile, enabledExts

         -- * Disco monad

       , Disco

         -- ** Utilities
       , io, iputStrLn, iputStr, iprint
       , emitMessage, info, warning, err, panic, debug
       , runDisco, catchAsMessage, catchAndPrintErrors

         -- ** Memory/environment utilities
       , allocate, delay, delay', mkValue, mkSimple

         -- ** Top level phases
       , parseDiscoModule
       , typecheckDisco
       , loadDiscoModule

       )
       where

import           Control.Lens                            (makeLenses, use, (%=),
                                                          (<+=), (<>=))
import           Control.Monad.Except                    (MonadError,
                                                          catchError,
                                                          throwError)
import           Control.Monad.Fail                      (MonadFail)
import qualified Control.Monad.Fail                      as Fail
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.IntMap.Lazy                        (IntMap)
import qualified Data.IntMap.Lazy                        as IntMap
import           Data.IntSet                             (IntSet)
import qualified Data.IntSet                             as IntSet
import qualified Data.Map                                as M
import qualified Data.Sequence                           as Seq
import qualified Data.Set                                as S
import           Data.Void
import           System.FilePath                         ((-<.>))
import           Text.Megaparsec                         hiding (runParser)

import           Unbound.Generics.LocallyNameless

import           System.Console.Haskeline.MonadException

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.Context
import           Disco.Extensions
import           Disco.Messages
import           Disco.Module
import           Disco.Parser
import           Disco.Typecheck                         (checkModule)
import           Disco.Typecheck.Monad
import           Disco.Types

------------------------------------------------------------
-- Values
------------------------------------------------------------

-- | The type of values produced by the interpreter.
data Value where
  -- | A numeric value, which also carries a flag saying how
  --   fractional values should be diplayed.
  VNum   :: RationalDisplay -> Rational -> Value

  -- | A constructor with arguments.  The Int indicates which
  --   constructor it is.  For example, False is represented by
  --   @VCons 0 []@, and True by @VCons 1 []@.  A pair is
  --   represented by @VCons 0 [v1, v2]@, and @inr v@ by @VCons 1
  --   [v]@.
  VCons  :: Int -> [Value] -> Value

  -- | A built-in function constant.
  VConst :: Op -> Value

  -- | A closure, i.e. a function body together with its
  --   environment.
  VClos  :: Bind [Name Core] Core -> Env -> Value

  -- | A partial application, i.e. an application of a thing to some
  --   arguments which is still waiting for more.  Invariant: the
  --   thing being applied is in WHNF.
  VPAp   :: Value -> [Value] -> Value

  -- | A thunk, i.e. an unevaluated core expression together with
  --   its environment.
  VThunk :: Core -> Env -> Value

  -- | An indirection, i.e. a pointer to an entry in the value table.
  --   This is how we get graph reduction.  When we create a thunk, we
  --   put it in a new entry in the value table, and return a VIndir.
  --   The VIndir can get copied but all the copies refer to the same
  --   thunk, which will only be evaluated once, the first time the
  --   value is demanded.
  VIndir :: Loc -> Value

  -- | A literal function value.  @VFun@ is only used when
  --   enumerating function values in order to decide comparisons at
  --   higher-order function types.  For example, in order to
  --   compare two values of type @(Bool -> Bool) -> Bool@ for
  --   equality, we have to enumerate all functions of type @Bool ->
  --   Bool@ as @VFun@ values.
  --
  --   We assume that all @VFun@ values are /strict/, that is, their
  --   arguments should be fully evaluated to RNF before being
  --   passed to the function.
  VFun_   :: ValFun -> Value

  -- | A @Disco Value@ computation which can be run later, along with
  --   the environment in which it should run, and a set of referenced
  --   memory locations that should not be garbage collected.
  VDelay_  :: ValDelay -> IntSet -> Env -> Value

  -- | A literal bag, containing a finite list of (perhaps only
  --   partially evaluated) values, each paired with a count.  This is
  --   also used to represent sets (with the invariant that all counts
  --   are equal to 1).
  VBag :: [(Value, Integer)] -> Value

  -- | A disco type can be a value.  For now, there are only a very
  --   limited number of places this could ever show up (in
  --   particular, as an argument to @enumerate@ or @count@).
  VType :: Type -> Value
  deriving Show

-- | A @ValFun@ is just a Haskell function @Value -> Disco IErr
--   Value@.  It is a @newtype@ just so we can have a custom @Show@
--   instance for it and then derive a @Show@ instance for the rest of
--   the @Value@ type.
newtype ValFun = ValFun (Value -> Disco IErr Value)

instance Show ValFun where
  show _ = "<fun>"

pattern VFun :: (Value -> Disco IErr Value) -> Value
pattern VFun f = VFun_ (ValFun f)

-- | A @ValDelay@ is just a @Disco Value@ computation.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValDelay = ValDelay (Disco IErr Value)

instance Show ValDelay where
  show _ = "<delay>"

pattern VDelay :: Disco IErr Value -> IntSet -> Env -> Value
pattern VDelay m ls e = VDelay_ (ValDelay m) ls e

------------------------------------------------------------
-- Environments
------------------------------------------------------------

-- | An environment is a mapping from names to values.
type Env  = Ctx Core Value

-- | Locally extend the environment with a new name -> value mapping,
--   (shadowing any existing binding for the given name).
extendEnv :: Name Core -> Value -> Disco e a -> Disco e a
extendEnv x v = avoid [AnyName x] . extend x v

-- | Locally extend the environment with another environment.
--   Bindings in the new environment shadow bindings in the old.
extendsEnv :: Env -> Disco e a -> Disco e a
extendsEnv e' = avoid (map AnyName (names e')) . extends e'

-- | Get the current environment.
getEnv :: Disco e Env
getEnv = ask

-- | Run a @Disco@ computation with a /replaced/ (not extended)
--   environment.  This is used for evaluating things such as closures
--   and thunks that come with their own environment.
withEnv :: Env -> Disco e a -> Disco e a
withEnv = local . const

------------------------------------------------------------
-- Memory cells
------------------------------------------------------------

data Cell = Cell { cellVal :: Value, cellIsWHNF :: Bool }
  deriving (Show)

mkCell :: Value -> Cell
mkCell v = Cell v False

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | Errors that can be generated during interpreting.
data IErr where

  -- | Module not found.
  ModuleNotFound :: ModName -> IErr

  -- | Cyclic import encountered.
  CyclicImport :: ModName -> IErr

  -- | Error encountered during typechecking.
  TypeCheckErr :: TCError -> IErr

  -- | Error encountered during parsing.
  ParseErr :: ParseErrorBundle String Data.Void.Void -> IErr

  -- | An unbound name.
  UnboundError  :: Name Core -> IErr

  -- | An unknown prim name.
  UnknownPrim   :: String    -> IErr

  -- | v should be a number, but isn't.
  NotANum       :: Value     -> IErr

  -- | Division by zero.
  DivByZero     ::              IErr

  -- | Underflow, e.g. (2 - 3 : Nat)
  Underflow     ::              IErr

  -- | Overflow, e.g. (2^66)!
  Overflow      ::              IErr

  -- | Taking the base-2 logarithm of zero.
  LgOfZero      ::              IErr

  -- | v should be a boolean, but isn't.
  NotABool      :: Value     -> IErr

  -- | Non-exhaustive case analysis.
  NonExhaustive ::              IErr

  -- | Trying to count an infinite type.
  InfiniteTy    :: Type      -> IErr

  -- | Internal error for features not yet implemented.
  Unimplemented :: String    -> IErr

  -- | User-generated crash.
  Crash         :: String    -> IErr

  deriving Show

------------------------------------------------------------
-- Disco monad state
------------------------------------------------------------

-- | A location in memory is represented by an @Int@.
type Loc = Int

-- | The various pieces of state tracked by the 'Disco' monad.
data DiscoState e = DiscoState
  {
    _topCtx        :: Ctx Term Sigma
    -- ^ Top-level type environment.

  , _topDefns      :: Ctx Core Core
    -- ^ Environment of top-level definitions.  Set by 'loadDefs'.

  , _topTyDefns    :: TyDefCtx
    -- ^ Environment of top-level type definitions.

  , _topEnv        :: Env
    -- ^ Top-level environment mapping names to values (which all
    --   start as indirections to thunks).  Set by 'loadDefs'.
    --   Use it when evaluating with 'withTopEnv'.

  , _topDocs       :: Ctx Term Docs
    -- ^ Top-level documentation.

  , _memory        :: IntMap Cell
    -- ^ A memory is a mapping from "locations" (uniquely generated
    --   identifiers) to values, along with a flag saying whether the
    --   value has been evaluated yet.  It also keeps track of the
    --   next unused location.  We keep track of a memory during
    --   evaluation, and can create new memory locations to store
    --   things that should only be evaluated once.

  , _nextLoc       :: Loc
    -- ^ The next available (unused) memory location.

  , _reachableLocs :: IntSet
    -- ^ The memory locations found to be reachable during a garbage
    --   collection pass.

  , _messageLog    :: MessageLog e
    -- ^ A stream of messages generated by the system.

  , _lastFile      :: Maybe FilePath
    -- ^ The most recent file which was :loaded by the user.

  , _enabledExts   :: ExtSet
    -- ^ The set of language extensions currently enabled in the REPL.
    --   Note this affects only expressions entered at the REPL
    --   prompt, not modules loaded into the REPL; each module
    --   specifies its own extensions.
  }

-- | The initial state for the @Disco@ monad.
initDiscoState :: DiscoState e
initDiscoState = DiscoState
  { _topCtx        = emptyCtx
  , _topDefns      = emptyCtx
  , _topTyDefns    = M.empty
  , _topDocs       = emptyCtx
  , _topEnv        = emptyCtx
  , _memory        = IntMap.empty
  , _nextLoc       = 0
  , _reachableLocs = IntSet.empty
  , _messageLog    = emptyMessageLog
  , _lastFile      = Nothing
  , _enabledExts   = defaultExts
  }

------------------------------------------------------------
-- Disco monad
------------------------------------------------------------

-- | The main monad used by the Disco REPL, and by the interpreter and
--   pretty printer.
--
--   * Keeps track of state such as current top-level definitions,
--     types, and documentation, and memory for allocation of thunks.
--   * Keeps track of a read-only environment for binding local
--     variables to their values.
--   * Can throw exceptions of type @e@
--   * Can log messages (errors, warnings, etc.)
--   * Can generate fresh names
--   * Can do I/O
type Disco e = StateT (DiscoState e) (ReaderT Env (ExceptT e (LFreshMT IO)))

------------------------------------------------------------
-- Some instances needed to ensure that Disco is an instance of the
-- MonadException class from haskeline.

-- Orphan instance, since it doesn't really seem sensible for either
-- unbound-generics or haskeline to depend on the other.
instance MonadException m => MonadException (LFreshMT m) where
  controlIO f = LFreshMT $ controlIO $ \(RunIO run) -> let
                  run' = RunIO (fmap LFreshMT . run . unLFreshMT)
                  in unLFreshMT <$> f run'

-- This should eventually move into unbound-generics.
instance MonadFail m => MonadFail (LFreshMT m) where
  fail = LFreshMT . Fail.fail

-- We need this orphan instance too.  It would seem to make sense for
-- haskeline to provide an instance for ExceptT, but see:
--   https://github.com/judah/haskeline/issues/18
--   https://github.com/judah/haskeline/pull/22
--
-- Idris does the same thing,
--   https://github.com/idris-lang/Idris-dev/blob/5d7388bb3c71fe56b7c09c0b31a94d44bf9f4f25/src/Idris/Output.hs#L38

instance MonadException m => MonadException (ExceptT e m) where
  controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                  run' = RunIO (fmap ExceptT . run . runExceptT)
                  in runExceptT <$> f run'

------------------------------------------------------------
-- Lenses for state
------------------------------------------------------------

makeLenses ''DiscoState

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io i = liftIO i

iputStrLn :: MonadIO m => String -> m ()
iputStrLn = io . putStrLn

iputStr :: MonadIO m => String -> m ()
iputStr = io . putStr

iprint :: (MonadIO m, Show a) => a -> m ()
iprint = io . print

emitMessage :: MessageLevel -> MessageBody e -> Disco e ()
emitMessage lev body = messageLog <>= Seq.singleton (Message lev body)

info, warning, err, panic, debug :: MessageBody e -> Disco e ()
info    = emitMessage Info
warning = emitMessage Warning
err     = emitMessage Error
panic   = emitMessage Panic
debug   = emitMessage Debug


-- | Run a computation in the @Disco@ monad, starting in the empty
--   environment.
runDisco :: Disco e a -> IO (Either e a)
runDisco
  = runLFreshMT
  . runExceptT
  . flip runReaderT emptyCtx
  . flip evalStateT initDiscoState

-- | Run a @Disco@ computation; if it throws an exception, catch it
--   and turn it into a message.
catchAsMessage :: Disco e () -> Disco e ()
catchAsMessage m = m `catchError` (err . Item)

-- XXX eventually we should get rid of this and replace with catchAsMessage
catchAndPrintErrors :: a -> Disco IErr a -> Disco IErr a
catchAndPrintErrors a m = m `catchError` (\e -> handler e >> return a)
  where
    handler (ParseErr e)     = iputStrLn $ errorBundlePretty e
    handler (TypeCheckErr e) = iprint e
    handler e                = iprint e

------------------------------------------------------------
-- Memory/environment utilities
------------------------------------------------------------

-- | Allocate a new memory cell for the given value, and return its
--   'Loc'.
allocate :: Value -> Disco e Loc
allocate v = do
  loc <- nextLoc <+= 1
  -- io $ putStrLn $ "allocating " ++ show v ++ " at location " ++ show loc
  memory %= IntMap.insert loc (mkCell v)
  return loc

-- | Turn a value into a "simple" value which takes up a constant
--   amount of space: some are OK as they are; for others, we turn
--   them into an indirection and allocate a new memory cell for them.
mkSimple :: Value -> Disco e Value
mkSimple v@(VNum {})    = return v
mkSimple v@(VCons _ []) = return v
mkSimple v@(VConst _)   = return v
mkSimple v@(VClos {})   = return v
mkSimple v@(VType {})   = return v
mkSimple v@(VIndir {})  = return v
mkSimple v              = VIndir <$> allocate v

-- | Delay a @Disco e Value@ computation by packaging it into a
--   @VDelay@ constructor along with the current environment.
delay :: Disco IErr Value -> Disco IErr Value
delay = delay' []

-- | Like 'delay', but also specify a set of values which will be
--   needed during the delayed computation, to prevent any memory
--   referenced by the values from being garbage collected.
delay' :: [Value] -> Disco IErr Value -> Disco IErr Value
delay' vs imv = do
  ls <- getReachables vs
  VDelay imv ls <$> getEnv

-- | Turn a Core expression into a value.  Some kinds of expressions
--   can be turned into corresponding values directly; for others we
--   create a thunk by packaging up the @Core@ expression with the
--   current environment.  The thunk is stored in a new location in
--   memory, and the returned value consists of an indirection
--   referring to its location.
mkValue :: Core -> Disco e Value
mkValue (CConst op)  = return $ VConst op
mkValue (CCons i cs) = VCons i <$> mapM mkValue cs
mkValue (CNum d r)   = return $ VNum d r
mkValue (CType ty)   = return $ VType ty
mkValue c            = VIndir <$> (allocate =<< (VThunk c <$> getEnv))

-- | Run a computation with the top-level environment used as the
--   current local environment.  For example, this is used every time
--   we start evaluating an expression entered at the command line.
withTopEnv :: Disco e a -> Disco e a
withTopEnv m = do
  env <- use topEnv
  withEnv env m

-- | Deallocate any memory cells which are no longer referred to by
--   any top-level binding.
garbageCollect :: Disco e ()
garbageCollect = do

    --  reachableLocs .= IntSet.empty  yields some sort of ambiguity error...
  modify (\s -> s { _reachableLocs = IntSet.empty })

  env  <- use topEnv
  reachableEnv env
  keep <- use reachableLocs

  memory %= (\mem -> IntMap.withoutKeys mem (IntMap.keysSet mem `IntSet.difference` keep))

-- | Get the set of memory locations reachable from a set of values.
--   Note that this clobbers the 'reachableLocs' component of the
--   @Disco@ state.
getReachables :: [Value] -> Disco e IntSet
getReachables vs = do
  modify (\s -> s { _reachableLocs = IntSet.empty })
  reachables vs
  use reachableLocs

-- | Mark the memory locations reachable from the values stored in an
--   environment.
reachableEnv :: Env -> Disco e ()
reachableEnv = reachables . M.elems

-- | Mark the memory locations reachable from a value.
reachable :: Value -> Disco e ()
reachable (VCons _ vs)    = reachables vs
reachable (VClos _ e)     = reachableEnv e
reachable (VPAp v vs)     = reachables (v:vs)
reachable (VThunk _ e)    = reachableEnv e
reachable (VIndir l)      = reachableLoc l
reachable (VDelay _ ls e) = (reachableLocs %= IntSet.union ls) >> reachableEnv e
reachable (VBag vs)       = reachables (map fst vs)
reachable _               = return ()

-- | Mark the memory locations reachable from a set of values.
reachables :: [Value] -> Disco e ()
reachables = mapM_ reachable

-- | Mark the given memory location, and continue recursively marking
--   anything reachable from the value stored at this location if it
--   wasn't already visited.
reachableLoc :: Loc -> Disco e ()
reachableLoc l = do
  reach <- use reachableLocs
  case IntSet.member l reach of
    True -> return ()
    False -> do
      reachableLocs %= IntSet.insert l
      mem <- use memory
      case IntMap.lookup l mem of
        Nothing         -> return ()
        Just (Cell v _) -> reachable v

------------------------------------------------------------
-- High-level disco phases
------------------------------------------------------------

-- | Utility function: given an 'Either', wrap a 'Left' in the given
--   function and throw it as a 'Disco' error, or return a 'Right'.
adaptError :: MonadError e2 m => (e1 -> e2) -> Either e1 a -> m a
adaptError f = either (throwError . f) return

-- | Parse a module from a file, re-throwing a parse error if it
--   fails.
parseDiscoModule :: (MonadError IErr m, MonadIO m) => FilePath -> m Module
parseDiscoModule file = do
  str <- io $ readFile file
  adaptError ParseErr $ runParser wholeModule file str

-- | Run a typechecking computation, re-throwing a wrapped error if it
--   fails.
typecheckDisco :: MonadError IErr m => TyCtx -> TyDefCtx -> TCM a -> m a
typecheckDisco tyctx tydefs tcm =
  adaptError TypeCheckErr $ evalTCM (withTyDefns tydefs . extends tyctx $ tcm)

-- | Recursively loads a given module by first recursively loading and
--   typechecking its imported modules, adding the obtained
--   'ModuleInfo' records to a map from module names to info records,
--   and then typechecking the parent module in an environment with
--   access to this map. This is really just a depth-first search.
loadDiscoModule :: (MonadError IErr m, MonadIO m) => FilePath -> S.Set ModName -> ModName -> StateT (M.Map ModName ModuleInfo) m ModuleInfo
loadDiscoModule directory inProcess modName  = do
  when (S.member modName inProcess) (throwError $ CyclicImport modName)
  modMap <- get
  case M.lookup modName modMap of
    Just mi -> return mi
    Nothing -> do
      file <-resolveModule directory modName
             >>= maybe (throwError $ ModuleNotFound modName) return
      io . putStrLn $ "Loading " ++ (modName -<.> "disco") ++ "..."
      cm@(Module _ mns _ _) <- lift $ parseDiscoModule file

      -- mis only contains the module info from direct imports.
      mis <- mapM (loadDiscoModule directory (S.insert modName inProcess)) mns
      imports@(ModuleInfo _ _ tyctx tydefns _) <- adaptError TypeCheckErr $ combineModuleInfo mis
      m  <- lift $ typecheckDisco tyctx tydefns (checkModule cm)
      m' <- adaptError TypeCheckErr $ combineModuleInfo [imports, m]
      modify (M.insert modName m')
      return m'

-------------------------------------
--Tries
--
-- data VTrie a where
--   SumTrie :: IntMap (VTrie a) -> VTrie a
--   ProdTrie :: VTrie (VTrie a) -> VTrie a
--   Singleton :: [Value] -> a -> VTrie a
--   Empty :: VTrie a
--
-- -- trieLookup :: Value -> VTrie a -> Disco IErr (Maybe a)
-- -- trieLookup _ Empty = return Nothing
-- -- trieLookup (VNum _ r) (SumTrie m) = do
-- --   let maybeTrie = IntMap.lookup (numerator r) m
-- --   case maybeTrie of
-- --     Just (SumTrie m') -> do
-- --        let maybeSingleton = IntMap.lookup (denominator r) m'
-- --        case maybeSingleton of
-- --          Just (Singleton [] a) -> return $ Just a
-- --          _                     -> return Nothing
-- --     Nothing -> return Nothing
--
--
-- class TrieKey k where
--   adjustKey :: k -> (Maybe a -> Maybe a) -> VTrie a -> Disco IErr (Maybe a, VTrie a)
--   buildTrie :: k -> a -> Disco IErr (VTrie a)
--
-- insertKey :: (TrieKey k) => k -> a -> VTrie a -> Disco IErr (VTrie a)
-- insertKey k a t = adjustKey k (const (Just a)) t
--
-- lookupKey :: k -> VTrie a -> Disco IErr (Maybe a, VTrie a)
-- lookupKey k t = adjustKey k id t
--
-- instance TrieKey () where
--   adjustKey () f Empty = return $ (Nothing, maybe Empty Leaf (f Nothing))
--   adjustKey () f (Leaf a) = return $ (Just a, maybe Empty Leaf (f (Just a)))
--
--   buildTrie () = Leaf
--
-- pattern Leaf a = Singleton [] a
--
-- expandSingleton :: Value -> [Value] -> a -> Disco IErr (VTrie a)
-- expandSingleton v vs a = do
--   t <- buildTrie v (Singleton vs a)
--   return $ ProdTrie t
--
-- instance (TrieKey j, TrieKey k) => TrieKey (j,k) where
--   adjustKey (j,k) f Empty = case f Nothing of
--     Nothing -> return (Nothing, Empty)
--     Just a  -> do
--       newTrie <- buildTrie (j,k) a
--       return (Nothing, newTrie)
--   adjustKey (j,k) f (Singleton (v:vs) a) = do
--     newTrie <- expandSingleton v vs a
--     adjustKey (j,k) f newTrie
--   adjustKey (j,k) f (ProdTrie t) = do
--     let g Nothing  = fmap (buildTrie k) (f Nothing)
--         g (Just u) = adjustKey k f u
--     (mTrie, t') <- adjustKey j g t
--     case mTrie of
--       Nothing -> return (Nothing, t')
--       Just u  -> do
--         (kValue, u') <- lookupKey k u
--         updatedTrie <- ProdTrie $ insertKey j u' t'
--         return $ (kValue, updatedTrie)

--adjustKey (j,k)
