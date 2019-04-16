-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for things entered at an interactive disco prompt.
--
-----------------------------------------------------------------------------

module Disco.Interactive.Eval where

import           System.Console.Haskeline                as H
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Control.Arrow                           ((&&&))
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict
import           Data.Coerce
import qualified Data.Map                                as M
import qualified Data.Set                                as S
import           System.FilePath                         (splitFileName)

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval
import           Disco.Extensions
import           Disco.Interactive.Parser
import           Disco.Interpret.Core
import           Disco.Module
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Typecheck.Monad
import           Disco.Types

------------------------------------------------------------

import qualified Data.IntMap as IM
import Data.List (intercalate)

showVal :: Int -> Value -> String
showVal 0 _ = "_"
showVal _ (VNum _ r)   = show r
showVal k (VCons i vs) = "K" ++ show i ++ " [" ++ intercalate "," (map (showVal (k-1)) vs) ++ "]"
showVal _ (VConst op)  = show op
showVal _ (VClos _ _)  = "<closure>"
showVal _ (VPAp _ _ )  = "<pap>"
showVal _ (VThunk _ _) = "<thunk>"
showVal _ (VIndir l)   = "-> " ++ show l
showVal _ (VFun _)     = "<fun>"
showVal _ (VDelay _)   = "<delay>"
showVal _ (VBag _)     = "<bag>"
showVal _ (VType _)    = "<type>"

handleCMD :: String -> Disco IErr ()
handleCMD "" = return ()
handleCMD s = do
    exts <- use enabledExts
    case (parseLine exts s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l `catchError` (io . print  {- XXX pretty-print error -})

    -- env <- use topEnv
    -- mem <- use memory

    -- io $ print env

    -- forM_ (IM.assocs mem) $ \(k,v) ->
    --   io $ putStrLn $ show k ++ ": " ++ showVal 3 v
  where
    handleLine :: REPLExpr -> Disco IErr ()

    handleLine (Using e)     = enabledExts %= addExtension e
    handleLine (Let x t)     = handleLet x t
    handleLine (TypeCheck t) = handleTypeCheck t        >>= iputStrLn
    handleLine (Eval t)      = evalTerm t
    handleLine (ShowDefn x)  = handleShowDefn x         >>= iputStrLn
    handleLine (Parse t)     = iprint $ t
    handleLine (Pretty t)    = renderDoc (prettyTerm t) >>= iputStrLn
    handleLine (Ann t)       = handleAnn t              >>= iputStrLn
    handleLine (Desugar t)   = handleDesugar t          >>= iputStrLn
    handleLine (Compile t)   = handleCompile t          >>= iputStrLn
    handleLine (Load file)   = handleLoad file >> lastFile .= Just file >>return ()
    handleLine (Reload)      = do
      file <- use lastFile
      case file of
        Nothing -> iputStrLn "No file to reload."
        Just f  -> handleLoad f >> return()
    handleLine (Doc x)       = handleDocs x
    handleLine Nop           = return ()
    handleLine Help          = iputStrLn "Help!"

handleLet :: Name Term -> Term -> Disco IErr ()
handleLet x t = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  let mat = evalTCM (extends ctx $ withTyDefns tymap $ inferTop t)
  case mat of
    Left e -> io.print $ e   -- XXX pretty print
    Right (at, sig) -> do
      let c = compileTerm at
      thnk <- mkValue c
      topCtx   %= M.insert x sig
      topDefns %= M.insert (coerce x) c
      topEnv   %= M.insert (coerce x) thnk

handleShowDefn :: Name Term -> Disco IErr String
handleShowDefn x = do
  defns <- use topDefns
  case M.lookup (coerce x) defns of
    Nothing -> return $ "No definition for " ++ show x
    Just d  -> return $ show d

handleAnn :: Term -> Disco IErr String
handleAnn t = do
  case evalTCM (inferTop t) of
    Left e       -> return . show $ e
    Right (at,_) -> return . show $ at

handleDesugar :: Term -> Disco IErr String
handleDesugar t = do
  case evalTCM (inferTop t) of
    Left e       -> return.show $ e
    Right (at,_) -> renderDoc . prettyTerm . eraseDTerm . runDSM . desugarTerm $ at

handleCompile :: Term -> Disco IErr String
handleCompile t = do
  case evalTCM (inferTop t) of
    Left e       -> return.show $ e
    Right (at,_) -> return.show.compileTerm $ at

loadFile :: FilePath -> Disco IErr (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
handleLoad :: FilePath -> Disco IErr Bool
handleLoad fp = catchAndPrintErrors False $ do
  let (directory, modName) = splitFileName fp
  modMap <- execStateT (loadDiscoModule directory S.empty modName) M.empty
  let m@(ModuleInfo _ props _ _ _) = modMap M.! modName
  addModInfo m
  t <- withTopEnv $ runAllTests props
  io . putStrLn $ "Loaded."
  return t

-- | Added information from ModuleInfo to the Disco monad. This includes updating the
--   Disco monad with new term definitions, documentation, types, and type definitions.
addModInfo :: ModuleInfo -> Disco IErr ()
addModInfo (ModuleInfo docs _ tys tyds tmds) = do
  let cdefns = M.mapKeys coerce $ fmap compileDefn tmds
  topDocs  .= docs
  topCtx   .= tys
  topTyDefns .= tyds
  loadDefs cdefns
  return ()

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing.  Then move it
-- to the Property module.
runAllTests :: Ctx ATerm [AProperty] -> Disco IErr Bool  -- (Ctx ATerm [TestResult])
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = do
      io $ putStrLn "Running tests..."
      and <$> mapM (uncurry runTests) (M.assocs aprops)
      -- XXX eventually this should be moved into Disco.Property and
      -- use a logging framework?

  where
    numSamples :: Int
    numSamples = 50   -- XXX make this configurable somehow

    runTests :: Name ATerm -> [AProperty] -> Disco IErr Bool
    runTests n props = do
      iputStr ("  " ++ name2String n ++ ":")
      results <- sequenceA . fmap sequenceA $ map (id &&& runTest numSamples) props
      let failures = filter (not . testIsOK . snd) results
      case null failures of
        True  -> iputStrLn " OK"
        False -> do
          iputStrLn ""
          forM_ failures (uncurry prettyTestFailure)
      return (null failures)

-- XXX redo with message framework, with proper support for indentation etc.
-- XXX also move it to Property or Pretty or somewhere like that
prettyTestFailure :: AProperty -> TestResult -> Disco IErr ()
prettyTestFailure _ (TestOK {}) = return ()
prettyTestFailure prop (TestFalse env) = do
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStr "  - Test is false: " >> iputStrLn dp
  let qTys = M.fromList . fst . unsafeUnbind $ prop
  prettyCounterexample qTys env
prettyTestFailure prop (TestEqualityFailure ty v1 v2 env) = do
  iputStr     "  - Test result mismatch for: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    - Expected: " >> prettyValue ty v2
  iputStr     "    - But got:  " >> prettyValue ty v1
  let qTys = M.fromList . fst . unsafeUnbind $ prop
  prettyCounterexample qTys env
prettyTestFailure prop (TestRuntimeFailure e) = do
  iputStr     "  - Test failed: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    " >> iprint e

-- XXX comment, move somewhere else
prettyCounterexample :: Ctx ATerm Type -> Env -> Disco IErr ()
prettyCounterexample ctx env
  | M.null env = return ()
  | otherwise  = do
      iputStrLn "    Counterexample:"
      let maxNameLen = maximum . map (length . name2String) $ M.keys env
      mapM_ (prettyBind maxNameLen) $ M.assocs env
  where
    prettyBind maxNameLen (x,v) = do
      iputStr "      "
      iputStr =<< (renderDoc . prettyName $ x)
      iputStr (replicate (maxNameLen - length (name2String x)) ' ')
      iputStr " = "
      prettyValue (ctx !? coerce x) v
    m !? k = case M.lookup k m of
      Just val -> val
      Nothing  -> error $ "Failed M.! with key " ++ show k ++ " in map " ++ show m

handleDocs :: Name Term -> Disco IErr ()
handleDocs x = do
  ctx  <- use topCtx
  docs <- use topDocs
  case M.lookup x ctx of
    Nothing -> io . putStrLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettySigma ty]
      io . putStrLn $ p
      case M.lookup x docs of
        Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
        _                       -> return ()

evalTerm :: Term -> Disco IErr ()
evalTerm t = do
  ctx   <- use topCtx
  tymap <- use topTyDefns
  case evalTCM (extends ctx $ withTyDefns tymap $ inferTop t) of
    Left e   -> iprint e    -- XXX pretty-print
    Right (at,_) ->
      let ty = getType at
          c  = compileTerm at
      in (withTopEnv $ mkValue c) >>= prettyValue ty

handleTypeCheck :: Term -> Disco IErr String
handleTypeCheck t = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
    Left e        -> return.show $ e    -- XXX pretty-print
    Right (_,sig) -> renderDoc $ prettyTerm t <+> text ":" <+> prettySigma sig

