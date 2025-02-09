{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  Disco.Interactive.Commands
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Defining and dispatching all commands/functionality available at
-- the REPL prompt.
module Disco.Interactive.Commands (
  dispatch,
  discoCommands,
  handleLoad,
  loadFile,
  parseLine,
) where

import Control.Arrow ((&&&))
import Control.Lens (
  to,
  view,
  (%~),
  (.~),
  (?~),
  (^.),
 )
import Control.Monad (forM_, void, when)
import Control.Monad.Except ()
import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Coerce
import Data.List (find, isPrefixOf, sortBy, transpose)
import Data.List.NonEmpty qualified as NE
import Data.Map ((!))
import Data.Map qualified as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Typeable
import Disco.AST.Surface
import Disco.AST.Typed
import Disco.Compile
import Disco.Context as Ctx
import Disco.Desugar
import Disco.Doc
import Disco.Effects.Fresh (runFresh)
import Disco.Effects.Input
import Disco.Effects.LFresh
import Disco.Effects.State
import Disco.Enumerate (enumerateType)
import Disco.Error
import Disco.Eval
import Disco.Extensions
import Disco.Interpret.CESK
import Disco.Messages
import Disco.Module
import Disco.Names
import Disco.Parser (
  Parser,
  ident,
  reservedOp,
  runParser,
  sc,
  symbol,
  term,
  wholeModule,
  withExts,
 )
import Disco.Pretty hiding (empty, (<>))
import Disco.Pretty qualified as PP
import Disco.Property (prettyTestResult)
import Disco.Syntax.Operators
import Disco.Syntax.Prims (
  Prim (PrimAbs, PrimBOp, PrimCeil, PrimFloor, PrimUOp),
  toPrim,
 )
import Disco.Typecheck
import Disco.Typecheck.Erase
import Disco.Types
import Disco.Util (maximum0)
import Disco.Value
import Polysemy
import Polysemy.Error hiding (try)
import Polysemy.Output
import Polysemy.Reader
import System.FilePath (splitFileName)
import Text.Megaparsec hiding (State, runParser)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.PrettyPrint.Boxes qualified as B
import Unbound.Generics.LocallyNameless (
  Name,
  name2String,
  string2Name,
 )
import Prelude as P

------------------------------------------------------------
-- REPL expression type
------------------------------------------------------------

-- | Data type to represent things typed at the Disco REPL.  Each
--   constructor has a singleton type to facilitate dispatch.
data REPLExpr :: CmdTag -> * where
  TypeCheck :: Term -> REPLExpr 'CTypeCheck -- Typecheck a term
  Eval :: Module -> REPLExpr 'CEval -- Evaluate a block
  TestProp :: Term -> REPLExpr 'CTestProp -- Run a property test
  ShowDefn :: Name Term -> REPLExpr 'CShowDefn -- Show a variable's definition
  Parse :: Term -> REPLExpr 'CParse -- Show the parsed AST
  Pretty :: Term -> REPLExpr 'CPretty -- Pretty-print a term
  Print :: Term -> REPLExpr 'CPrint -- Print a string
  Table :: Term -> REPLExpr 'CTable -- Print a table
  Ann :: Term -> REPLExpr 'CAnn -- Show type-annotated term
  Desugar :: Term -> REPLExpr 'CDesugar -- Show a desugared term
  Compile :: Term -> REPLExpr 'CCompile -- Show a compiled term
  Load :: FilePath -> REPLExpr 'CLoad -- Load a file.
  Reload :: REPLExpr 'CReload -- Reloads the most recently
  -- loaded file.
  Doc :: DocInput -> REPLExpr 'CDoc -- Show documentation.
  Nop :: REPLExpr 'CNop -- No-op, e.g. if the user
  -- just enters a comment
  Help :: REPLExpr 'CHelp -- Show help
  Names :: REPLExpr 'CNames -- Show bound names

deriving instance Show (REPLExpr c)

-- | An existential wrapper around any REPL expression.
data SomeREPLExpr where
  SomeREPL :: Typeable c => REPLExpr c -> SomeREPLExpr

------------------------------------------------------------
-- REPL command types
------------------------------------------------------------

data REPLCommandCategory
  = -- | REPL commands for everyday users
    User
  | -- | REPL commands for developers working on Disco
    Dev
  deriving (Eq, Show)

data REPLCommandType
  = -- | Things that don't start with a colon: eval and nop
    BuiltIn
  | -- | Things that start with a colon, e.g. :help, :names, :load...
    ColonCmd
  deriving (Eq, Show)

-- | Tags used at the type level to denote each REPL command.
data CmdTag
  = CTypeCheck
  | CEval
  | CShowDefn
  | CParse
  | CPretty
  | CPrint
  | CTable
  | CAnn
  | CDesugar
  | CCompile
  | CLoad
  | CReload
  | CDoc
  | CNop
  | CHelp
  | CNames
  | CTestProp
  deriving (Show, Eq, Typeable)

------------------------------------------------------------
-- REPL command info record
------------------------------------------------------------

-- | Data type to represent all the information about a single REPL
--   command.
data REPLCommand (c :: CmdTag) = REPLCommand
  { name :: String
  -- ^ Name of the command
  , helpcmd :: String
  -- ^ Help text showing how to use the command, e.g. ":ann <term>"
  , shortHelp :: String
  -- ^ Short free-form text explaining the command.
  --   We could also consider adding long help text as well.
  , category :: REPLCommandCategory
  -- ^ Is the command for users or devs?
  , cmdtype :: REPLCommandType
  -- ^ Is it a built-in command or colon command?
  , action :: REPLExpr c -> (forall r. Members DiscoEffects r => Sem r ())
  -- ^ The action to execute,
  -- given the input to the
  -- command.
  , parser :: Parser (REPLExpr c)
  -- ^ Parser for the command argument(s).
  }

-- | An existential wrapper around any REPL command info record.
data SomeREPLCommand where
  SomeCmd :: Typeable c => REPLCommand c -> SomeREPLCommand

------------------------------------------------------------
-- REPL command lists
------------------------------------------------------------

type REPLCommands = [SomeREPLCommand]

-- | Keep only commands of a certain type.
byCmdType :: REPLCommandType -> REPLCommands -> REPLCommands
byCmdType ty = P.filter (\(SomeCmd rc) -> cmdtype rc == ty)

-- | Given a list of REPL commands and something typed at the REPL,
--   pick the first command with a matching type-level tag and run its
--   associated action.
dispatch :: Members DiscoEffects r => REPLCommands -> SomeREPLExpr -> Sem r ()
dispatch [] _ = return ()
dispatch (SomeCmd c : cs) r@(SomeREPL e) = case gcast e of
  Just e' -> outputDiscoErrors $ action c e'
  Nothing -> dispatch cs r

-- | The list of all commands that can be used at the REPL.
--   Resolution of REPL commands searches this list /in order/, which
--   means ambiguous command prefixes (e.g. :t for :type) are resolved
--   to the first matching command.
discoCommands :: REPLCommands
discoCommands =
  [ SomeCmd annCmd
  , SomeCmd compileCmd
  , SomeCmd desugarCmd
  , SomeCmd docCmd
  , SomeCmd evalCmd
  , SomeCmd helpCmd
  , SomeCmd loadCmd
  , SomeCmd namesCmd
  , SomeCmd nopCmd
  , SomeCmd parseCmd
  , SomeCmd prettyCmd
  , SomeCmd printCmd
  , SomeCmd tableCmd
  , SomeCmd reloadCmd
  , SomeCmd showDefnCmd
  , SomeCmd typeCheckCmd
  , SomeCmd testPropCmd
  ]

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

builtinCommandParser :: REPLCommands -> Parser SomeREPLExpr
builtinCommandParser =
  foldr ((<|>) . (\(SomeCmd rc) -> SomeREPL <$> try (parser rc))) empty
    . byCmdType BuiltIn

-- | Parse one of the colon commands in the given list of commands.
commandParser :: REPLCommands -> Parser SomeREPLExpr
commandParser allCommands =
  (symbol ":" *> many C.lowerChar) >>= parseCommandArgs allCommands

-- | Given a list of available commands and a string seen after a
--   colon, return a parser for its arguments.
parseCommandArgs :: REPLCommands -> String -> Parser SomeREPLExpr
parseCommandArgs allCommands cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
 where
  badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."

  parsers =
    map (\(SomeCmd rc) -> (name rc, SomeREPL <$> parser rc)) $
      byCmdType ColonCmd allCommands

-- | Parse a file name.
fileParser :: Parser FilePath
fileParser = many C.spaceChar *> some (escapedSpace <|> L.charLiteral <|> anySingle)
 where
  escapedSpace = try (C.char '\\' *> C.char ' ')

-- | A parser for something entered at the REPL prompt.
lineParser :: REPLCommands -> Parser SomeREPLExpr
lineParser allCommands =
  builtinCommandParser allCommands
    <|> commandParser allCommands

-- | Given a list of available REPL commands and the currently enabled
--   extensions, parse a string entered at the REPL prompt, returning
--   either a parse error message or a parsed REPL expression.
parseLine :: REPLCommands -> ExtSet -> String -> Either String SomeREPLExpr
parseLine allCommands exts s =
  case runParser (withExts exts (lineParser allCommands)) "" s of
    Left e -> Left $ errorBundlePretty e
    Right l -> Right l

--------------------------------------------------------------------------------
-- The commands!
--------------------------------------------------------------------------------

------------------------------------------------------------
-- :ann

annCmd :: REPLCommand 'CAnn
annCmd =
  REPLCommand
    { name = "ann"
    , helpcmd = ":ann"
    , shortHelp = "Show type-annotated typechecked term"
    , category = Dev
    , cmdtype = ColonCmd
    , action = inputToState @TopInfo . handleAnn
    , parser = Ann <$> term
    }

handleAnn ::
  Members '[Error DiscoError, Input TopInfo, Output (Message ())] r =>
  REPLExpr 'CAnn ->
  Sem r ()
handleAnn (Ann t) = do
  (at, _) <- typecheckTop $ inferTop1 t
  infoPretty at

------------------------------------------------------------
-- :compile

compileCmd :: REPLCommand 'CCompile
compileCmd =
  REPLCommand
    { name = "compile"
    , helpcmd = ":compile"
    , shortHelp = "Show a compiled term"
    , category = Dev
    , cmdtype = ColonCmd
    , action = inputToState @TopInfo . handleCompile
    , parser = Compile <$> term
    }

handleCompile ::
  Members '[Error DiscoError, Input TopInfo, Output (Message ())] r =>
  REPLExpr 'CCompile ->
  Sem r ()
handleCompile (Compile t) = do
  (at, _) <- typecheckTop $ inferTop1 t
  infoPretty . compileTerm $ at

------------------------------------------------------------
-- :desugar

desugarCmd :: REPLCommand 'CDesugar
desugarCmd =
  REPLCommand
    { name = "desugar"
    , helpcmd = ":desugar"
    , shortHelp = "Show a desugared term"
    , category = Dev
    , cmdtype = ColonCmd
    , action = inputToState @TopInfo . handleDesugar
    , parser = Desugar <$> term
    }

handleDesugar ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output (Message ())] r =>
  REPLExpr 'CDesugar ->
  Sem r ()
handleDesugar (Desugar t) = do
  (at, _) <- typecheckTop $ inferTop1 t
  info $ pretty' . eraseDTerm . runDesugar . desugarTerm $ at

------------------------------------------------------------
-- :doc

docCmd :: REPLCommand 'CDoc
docCmd =
  REPLCommand
    { name = "doc"
    , helpcmd = ":doc <term>"
    , shortHelp = "Show documentation"
    , category = User
    , cmdtype = ColonCmd
    , action = inputToState @TopInfo . handleDoc
    , parser = Doc <$> parseDoc
    }

-- An input to the :doc command can be either a term, a primitive
-- operator, or something else.
data DocInput = DocTerm Term | DocPrim Prim | DocOther String
  deriving (Show)

parseDoc :: Parser DocInput
parseDoc =
  (DocTerm <$> try term)
    <|> (DocPrim <$> try (parseNakedOpPrim <?> "operator"))
    <|> (DocOther <$> (sc *> many (anySingleBut ' ')))

handleDoc ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output (Message ())] r =>
  REPLExpr 'CDoc ->
  Sem r ()
handleDoc (Doc (DocTerm (TBool _))) = handleDocBool
handleDoc (Doc (DocTerm TUnit)) = handleDocUnit
handleDoc (Doc (DocTerm TWild)) = handleDocWild
handleDoc (Doc (DocTerm (TPrim p))) = handleDocPrim p
handleDoc (Doc (DocTerm (TVar x))) = handleDocVar x
handleDoc (Doc (DocTerm _)) =
  err "Can't display documentation for an expression.  Try asking about a function, operator, or type name."
handleDoc (Doc (DocPrim p)) = handleDocPrim p
handleDoc (Doc (DocOther s)) = handleDocMap (OtherKey s)

handleDocBool :: Members '[Output (Message ())] r => Sem r ()
handleDocBool =
  info $
    "T and F (also written true and false, or True and False) are the two possible values of type Boolean."
      $+$ formatReference (mkRef "bool")

handleDocUnit :: Members '[Output (Message ())] r => Sem r ()
handleDocUnit =
  info $
    "The unit value, i.e. the single value of type Unit."
      $+$ formatReference (mkRef "unit")

handleDocWild :: Members '[Output (Message ())] r => Sem r ()
handleDocWild =
  info $
    "A wildcard pattern."
      $+$ formatReference (mkRef "wild-pattern")

handleDocVar ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output (Message ())] r =>
  Name Term ->
  Sem r ()
handleDocVar x = do
  replCtx <- inputs @TopInfo (view (replModInfo . miTys))
  replTydefs <- inputs @TopInfo (view (replModInfo . miTydefs))
  replDocs <- inputs @TopInfo (view (replModInfo . miDocs))

  importCtx <- inputs @TopInfo (joinCtxs . map (view miTys) . M.elems . view topModMap)
  importTydefs <- inputs @TopInfo (M.unions . map (view miTydefs) . M.elems . view topModMap)
  importDocs <- inputs @TopInfo (joinCtxs . map (view miDocs) . M.elems . view topModMap)

  let ctx = replCtx `joinCtx` importCtx
      tydefs = importTydefs `M.union` replTydefs
      docs = replDocs `joinCtx` importDocs

  debug $ text . show $ docs

  case (Ctx.lookupAll' x ctx, M.lookup (name2String x) tydefs) of
    ([], Nothing) ->
      -- Maybe the variable name entered by the user is actually a prim.
      case toPrim (name2String x) of
        (prim : _) -> handleDocPrim prim
        _ -> err $ "No documentation found for '" <> pretty' x <> "'."
    (binds, def) ->
      mapM_ (showDoc docs) (map Left binds ++ map Right (maybeToList def))
 where
  showDoc docs (Left (qn, ty)) =
    info $
      hsep [pretty' x, ":", pretty' ty]
        $+$ case Ctx.lookup' qn docs of
          Just (DocString ss : _) -> vcat (text "" : map text ss ++ [text ""])
          _ -> PP.empty
  showDoc docs (Right tdBody) =
    info $
      pretty' (name2String x, tdBody)
        $+$ case Ctx.lookupAll' x docs of
          ((_, DocString ss : _) : _) -> vcat (text "" : map text ss ++ [text ""])
          _ -> PP.empty

handleDocPrim ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output (Message ())] r =>
  Prim ->
  Sem r ()
handleDocPrim prim = do
  handleTypeCheck (TypeCheck (TPrim prim))
  let attrs =
        ( case prim of
            PrimUOp u -> case uopMap ! u of
              OpInfo (UOpF f _) syns _ -> describeAlts (f == Post) (f == Pre) syns
              _ -> error $ "handleDocPrim: No OpInfo for unary op " ++ show u
            PrimBOp b -> describeAlts True True (opSyns $ bopMap ! b)
            PrimFloor -> describeAlts False False ["floor(x)", "⌊x⌋"]
            PrimCeil -> describeAlts False False ["ceiling(x)", "⌈x⌉"]
            PrimAbs -> describeAlts False False ["abs(x)", "|x|"]
            _ -> []
        )
          ++ ( case prim of
                PrimUOp u -> [describePrec (uPrec u)]
                PrimBOp b -> [describePrec (bPrec b) <> describeFixity (assoc b)]
                _ -> []
             )
  case attrs of
    [] -> pure ()
    _ -> info . vcat $ attrs
  info PP.empty
  handleDocMap (PrimKey prim)
 where
  describePrec p = "precedence level" <+> text (show p)
  describeFixity In = PP.empty
  describeFixity InL = ", left associative"
  describeFixity InR = ", right associative"
  describeAlts _ _ [] = []
  describeAlts _ _ [_] = []
  describeAlts pre post (_ : alts) = ["Alternative syntax:" <+> intercalate "," (map showOp alts)]
   where
    showOp op =
      hcat
        [ if pre then "~" else PP.empty
        , text op
        , if post then "~" else PP.empty
        ]

formatReference :: Reference -> Sem r (Doc ann)
formatReference (Reference rty p) = case rty of
  Ref -> "https://disco-lang.readthedocs.io/en/latest/reference/" <> text p <> ".html"
  Intro -> "https://disco-lang.readthedocs.io/en/latest/introduction/" <> text p <> ".html"
  URL -> text p

handleDocMap ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output (Message ())] r =>
  DocKey ->
  Sem r ()
handleDocMap k = case M.lookup k docMap of
  Nothing -> case k of
    PrimKey _ -> pure ()
    OtherKey s -> info $ "No documentation found for '" <> text s <> "'."
  Just (d, refs) ->
    info . vcat $
      [ text d
      , PP.empty
      ]
        ++ case refs of
          [] -> []
          _ -> map formatReference refs ++ [PP.empty]

------------------------------------------------------------
-- eval

evalCmd :: REPLCommand 'CEval
evalCmd =
  REPLCommand
    { name = "eval"
    , helpcmd = "<code>"
    , shortHelp = "Evaluate a block of code"
    , category = User
    , cmdtype = BuiltIn
    , action = handleEval
    , parser = Eval <$> wholeModule REPL
    }

handleEval ::
  Members (Error DiscoError ': State TopInfo ': Output (Message ()) ': Embed IO ': EvalEffects) r =>
  REPLExpr 'CEval ->
  Sem r ()
handleEval (Eval m) = do
  mi <- loadParsedDiscoModule False FromCwdOrStdlib REPLModule m
  addToREPLModule mi
  forM_ (mi ^. miTerms) (mapError EvalErr . evalTerm True . fst)

-- garbageCollect?

-- First argument = should the value be printed?
evalTerm :: Members (Error EvalError ': State TopInfo ': Output (Message ()) ': EvalEffects) r => Bool -> ATerm -> Sem r Value
evalTerm pr at = do
  env <- use @TopInfo topEnv
  v <- runInputConst env $ eval (compileTerm at)

  tydefs <- use @TopInfo (replModInfo . to allTydefs)
  when pr $ info $ runInputConst tydefs $ prettyValue' ty v

  modify @TopInfo $
    (replModInfo . miTys %~ Ctx.insert (QName (QualifiedName REPLModule) (string2Name "it")) (toPolyType ty))
      . (topEnv %~ Ctx.insert (QName (QualifiedName REPLModule) (string2Name "it")) v)
  return v
 where
  ty = getType at

------------------------------------------------------------
-- :help

helpCmd :: REPLCommand 'CHelp
helpCmd =
  REPLCommand
    { name = "help"
    , helpcmd = ":help"
    , shortHelp = "Show help"
    , category = User
    , cmdtype = ColonCmd
    , action = handleHelp
    , parser = return Help
    }

handleHelp :: Member (Output (Message ())) r => REPLExpr 'CHelp -> Sem r ()
handleHelp Help =
  info $
    vcat
      [ "Commands available from the prompt:"
      , text ""
      , vcat (map (\(SomeCmd c) -> showCmd c) $ sortedList discoCommands)
      , text ""
      ]
 where
  maxlen = longestCmd discoCommands
  sortedList cmds =
    sortBy (\(SomeCmd x) (SomeCmd y) -> compare (name x) (name y)) $ filteredCommands cmds
  showCmd c = text (padRight (helpcmd c) maxlen ++ "  " ++ shortHelp c)
  longestCmd cmds = maximum0 $ map (\(SomeCmd c) -> length $ helpcmd c) cmds
  padRight s maxsize = take maxsize (s ++ repeat ' ')
  --  don't show dev-only commands by default
  filteredCommands = P.filter (\(SomeCmd c) -> category c == User)

------------------------------------------------------------
-- :load

loadCmd :: REPLCommand 'CLoad
loadCmd =
  REPLCommand
    { name = "load"
    , helpcmd = ":load <filename>"
    , shortHelp = "Load a file"
    , category = User
    , cmdtype = ColonCmd
    , action = handleLoadWrapper
    , parser = Load <$> fileParser
    }

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
--   Disco.Interactive.CmdLine uses a version of this function that returns a Bool.
handleLoadWrapper ::
  Members (Error DiscoError ': State TopInfo ': Output (Message ()) ': Embed IO ': EvalEffects) r =>
  REPLExpr 'CLoad ->
  Sem r ()
handleLoadWrapper (Load fp) = void (handleLoad fp)

handleLoad ::
  Members (Error DiscoError ': State TopInfo ': Output (Message ()) ': Embed IO ': EvalEffects) r =>
  FilePath ->
  Sem r Bool
handleLoad fp = do
  let (directory, modName) = splitFileName fp

  -- Reset top-level module map and context to empty, so we start
  -- fresh and pick up any changes to imported modules etc.
  modify @TopInfo $ topModMap .~ M.empty
  modify @TopInfo $ topEnv .~ Ctx.emptyCtx

  -- Load the module.
  m <- inputToState @TopInfo $ loadDiscoModule False (FromDir directory) modName
  setREPLModule m

  -- Now run any tests
  t <- inputToState $ runAllTests (m ^. miNames) (m ^. miProps)

  -- Evaluate and print any top-level terms
  forM_ (m ^. miTerms) (mapError EvalErr . evalTerm True . fst)

  -- Remember which was the most recently loaded file, so we can :reload
  modify @TopInfo (lastFile ?~ fp)
  info "Loaded."
  return t

-- XXX Return a structured summary of the results, not a Bool; & move
-- this somewhere else?
runAllTests :: Members (Output (Message ()) ': Input TopInfo ': EvalEffects) r => [QName Term] -> Ctx ATerm [AProperty] -> Sem r Bool -- (Ctx ATerm [TestResult])
runAllTests declNames aprops
  | Ctx.null aprops = return True
  | otherwise = do
      info "Running tests..."
      -- Use the order the names were defined in the module
      and <$> mapM (uncurry runTests) (mapMaybe (\n -> (n,) <$> Ctx.lookup' (coerce n) aprops) declNames)
 where
  numSamples :: Int
  numSamples = 50 -- XXX make this configurable somehow
  runTests :: Members (Output (Message ()) ': Input TopInfo ': EvalEffects) r => QName Term -> [AProperty] -> Sem r Bool
  runTests (QName _ n) props = do
    results <- inputTopEnv $ traverse (sequenceA . (id &&& runTest numSamples)) props
    let failures = P.filter (not . testIsOk . snd) results
        hdr = pretty' n <> ":"

    case P.null failures of
      True -> info $ indent 2 $ hdr <+> "OK"
      False -> do
        tydefs <- inputs @TopInfo (view (replModInfo . to allTydefs))
        let prettyFailures =
              runInputConst tydefs
                . runReader initPA
                . runLFresh
                $ bulletList "-"
                $ map (uncurry prettyTestResult) failures
        info $ indent 2 $ hdr $+$ prettyFailures
    return (P.null failures)

------------------------------------------------------------
-- :names

namesCmd :: REPLCommand 'CNames
namesCmd =
  REPLCommand
    { name = "names"
    , helpcmd = ":names"
    , shortHelp = "Show all names in current scope"
    , category = User
    , cmdtype = ColonCmd
    , action = inputToState . handleNames
    , parser = return Names
    }

-- | Show names and types for each item in the top-level context.
handleNames ::
  Members '[Input TopInfo, LFresh, Output (Message ())] r =>
  REPLExpr 'CNames ->
  Sem r ()
handleNames Names = do
  tyDef <- inputs @TopInfo (view (replModInfo . miTydefs))
  ctx <- inputs @TopInfo (view (replModInfo . miTys))
  info $
    vcat (map pretty' (M.assocs tyDef))
      $+$ vcat (map showFn (Ctx.assocs ctx))
 where
  showFn (QName _ x, ty) = hsep [pretty' x, text ":", pretty' ty]

------------------------------------------------------------
-- nop

nopCmd :: REPLCommand 'CNop
nopCmd =
  REPLCommand
    { name = "nop"
    , helpcmd = ""
    , shortHelp = "No-op, e.g. if the user just enters a comment"
    , category = Dev
    , cmdtype = BuiltIn
    , action = handleNop
    , parser = Nop <$ (sc <* eof)
    }

handleNop :: REPLExpr 'CNop -> Sem r ()
handleNop Nop = pure ()

------------------------------------------------------------
-- :parse

parseCmd :: REPLCommand 'CParse
parseCmd =
  REPLCommand
    { name = "parse"
    , helpcmd = ":parse <expr>"
    , shortHelp = "Show the parsed AST"
    , category = Dev
    , cmdtype = ColonCmd
    , action = handleParse
    , parser = Parse <$> term
    }

handleParse :: Member (Output (Message ())) r => REPLExpr 'CParse -> Sem r ()
handleParse (Parse t) = info (text (show t))

------------------------------------------------------------
-- :pretty

prettyCmd :: REPLCommand 'CPretty
prettyCmd =
  REPLCommand
    { name = "pretty"
    , helpcmd = ":pretty <expr>"
    , shortHelp = "Pretty-print a term"
    , category = Dev
    , cmdtype = ColonCmd
    , action = handlePretty
    , parser = Pretty <$> term
    }

handlePretty :: Members '[LFresh, Output (Message ())] r => REPLExpr 'CPretty -> Sem r ()
handlePretty (Pretty t) = info $ pretty' t

------------------------------------------------------------
-- :print

printCmd :: REPLCommand 'CPrint
printCmd =
  REPLCommand
    { name = "print"
    , helpcmd = ":print <expr>"
    , shortHelp = "Print a string without the double quotes, interpreting special characters"
    , category = User
    , cmdtype = ColonCmd
    , action = handlePrint
    , parser = Print <$> term
    }

handlePrint :: Members (Error DiscoError ': State TopInfo ': Output (Message ()) ': EvalEffects) r => REPLExpr 'CPrint -> Sem r ()
handlePrint (Print t) = do
  at <- inputToState . typecheckTop $ checkTop t (toPolyType TyString)
  v <- mapError EvalErr . evalTerm False $ at
  info $ text (vlist vchar v)

------------------------------------------------------------
-- :table

tableCmd :: REPLCommand 'CTable
tableCmd =
  REPLCommand
    { name = "table"
    , helpcmd = ":table <expr>"
    , shortHelp = "Print a formatted table for a list or function"
    , category = User
    , cmdtype = ColonCmd
    , action = handleTable
    , parser = Table <$> parseTermOrOp
    }

handleTable :: Members (Error DiscoError ': State TopInfo ': Output (Message ()) ': EvalEffects) r => REPLExpr 'CTable -> Sem r ()
handleTable (Table t) = do
  (at, ty) <- inputToState . typecheckTop $ inferTop1 t
  v <- mapError EvalErr . evalTerm False $ at

  tydefs <- use @TopInfo (replModInfo . to allTydefs)
  info $ runInputConst tydefs $ formatTableFor ty v >>= text

-- | The max number of rows to show in the output of :table.
maxFunTableRows :: Int
maxFunTableRows = 25

-- | Uncurry a type, turning a type of the form A -> B -> ... -> Y ->
--   Z into the pair of types (A * B * ... * Y * Unit, Z).  Note we do
--   not optimize away the Unit at the end of the chain, since this
--   needs to be an isomorphism.  Otherwise we would not be able to
--   distinguish between e.g.  Z and Unit -> Z.
uncurryTy :: Type -> (Type, Type)
uncurryTy (tyA :->: tyB) = (tyA :*: tyAs, tyRes)
 where
  (tyAs, tyRes) = uncurryTy tyB
uncurryTy ty = (TyUnit, ty)

-- | Evaluate the application of a curried function to an uncurried
--   input.
evalCurried :: Members EvalEffects r => Type -> Value -> Type -> Value -> Sem r Value
evalCurried (_ :->: tyB) f (_ :*: tyY) v = do
  let (v1, v2) = vpair id id v
  f' <- evalApp f [v1]
  evalCurried tyB f' tyY v2
evalCurried _ v _ _ = return v

formatTableFor ::
  Members (LFresh ': Input TyDefCtx ': EvalEffects) r =>
  PolyType ->
  Value ->
  Sem r String
formatTableFor (Forall bnd) v = lunbind bnd $ \(vars, ty) ->
  case vars of
    [] -> case ty of
      TyList ety -> do
        byRows <- mapM (formatCols TopLevel ety) . vlist id $ v
        return $ renderTable byRows
      (_ :->: _) -> do
        let (tyInputs, tyRes) = uncurryTy ty
            vs = take (maxFunTableRows + 1) $ enumerateType tyInputs
            (tyInputs', stripV) = stripFinalUnit tyInputs
        results <- mapM (evalCurried ty v tyInputs) vs
        byRows <-
          mapM
            (formatCols TopLevel (tyInputs' :*: tyRes))
            (zipWith (curry (pairv id id)) (take maxFunTableRows (map stripV vs)) results)
        return $ renderTable (byRows ++ [[(B.left, "...")] | length vs == maxFunTableRows + 1])
      _otherTy -> do
        tyStr <- prettyStr ty
        return $ "Don't know how to make a table for type " ++ tyStr
    _vars -> return "Can't make a table for a polymorphic type"

-- | Strip the unit type from the end of a chain like (tA :*: (tB :*: (tC :*: Unit))),
--   which is an output of 'uncurryTy', and return a function to make the corresponding
--   change to a value of that type.
stripFinalUnit :: Type -> (Type, Value -> Value)
stripFinalUnit (tA :*: TyUnit) = (tA, fst . vpair id id)
stripFinalUnit (tA :*: tB) = (tA :*: tB', pairv id id . second v' . vpair id id)
 where
  (tB', v') = stripFinalUnit tB
stripFinalUnit ty = (ty, id)

data Level = TopLevel | NestedPair | InnerLevel
  deriving (Eq, Ord, Show)

-- | Turn a value into a list of formatted columns in a type-directed
--   way.  Lists and tuples are only split out into columns if they
--   occur at the top level; lists or tuples nested inside of other
--   data structures are simply pretty-printed.  However, note we have
--   to make a special case for nested tuples: if a pair type occurs
--   at the top level we keep recursively splitting out its children
--   into columns as long as they are also pair types.
--
--   Any value of a type other than a list or tuple is simply
--   pretty-printed.
formatCols ::
  (Member LFresh r, Member (Input TyDefCtx) r) =>
  Level ->
  Type ->
  Value ->
  Sem r [(B.Alignment, String)]
formatCols l (t1 :*: t2) (vpair id id -> (v1, v2))
  | l `elem` [TopLevel, NestedPair] =
      (++) <$> formatCols NestedPair t1 v1 <*> formatCols NestedPair t2 v2
-- Special case for String (= List Char), just print as string value
formatCols TopLevel TyString v = formatColDefault TyString v
-- For any other lists @ top level, print each element in a separate column
formatCols TopLevel (TyList ety) (vlist id -> vs) =
  concat <$> mapM (formatCols InnerLevel ety) vs
formatCols _ ty v = formatColDefault ty v

-- | Default formatting of a typed column value by simply
--   pretty-printing it, and using the alignment appropriate for its
--   type.
formatColDefault ::
  (Member (Input TyDefCtx) r, Member LFresh r) =>
  Type ->
  Value ->
  Sem r [(B.Alignment, String)]
formatColDefault ty v = (: []) . (alignmentForType ty,) <$> renderDoc (prettyValue ty v)

alignmentForType :: Type -> B.Alignment
alignmentForType ty | ty `elem` [TyN, TyZ, TyF, TyQ] = B.right
alignmentForType _ = B.left

-- | Render a table, given as a list of rows, formatting it so that
-- each column is aligned.
renderTable :: [[(B.Alignment, String)]] -> String
renderTable = stripTrailingWS . B.render . B.hsep 2 B.top . map renderCol . transpose . pad
 where
  pad :: [[(B.Alignment, String)]] -> [[(B.Alignment, String)]]
  pad rows = map (padTo (maximum0 . map length $ rows)) rows
  padTo n = take n . (++ repeat (B.left, ""))

  renderCol :: [(B.Alignment, String)] -> B.Box
  renderCol [] = B.nullBox
  renderCol ((align, x) : xs) = B.vcat align . map B.text $ x : map snd xs

  stripTrailingWS = unlines . map stripEnd . lines
  stripEnd = reverse . dropWhile isSpace . reverse

------------------------------------------------------------
-- :reload

reloadCmd :: REPLCommand 'CReload
reloadCmd =
  REPLCommand
    { name = "reload"
    , helpcmd = ":reload"
    , shortHelp = "Reloads the most recently loaded file"
    , category = User
    , cmdtype = ColonCmd
    , action = handleReload
    , parser = return Reload
    }

handleReload ::
  Members (Error DiscoError ': State TopInfo ': Output (Message ()) ': Embed IO ': EvalEffects) r =>
  REPLExpr 'CReload ->
  Sem r ()
handleReload Reload = do
  file <- use lastFile
  case file of
    Nothing -> info "No file to reload."
    Just f -> void (handleLoad f)

------------------------------------------------------------
-- :defn

showDefnCmd :: REPLCommand 'CShowDefn
showDefnCmd =
  REPLCommand
    { name = "defn"
    , helpcmd = ":defn <var>"
    , shortHelp = "Show a variable's definition"
    , category = User
    , cmdtype = ColonCmd
    , action = inputToState @TopInfo . handleShowDefn
    , parser = ShowDefn <$> (sc *> ident)
    }

handleShowDefn ::
  Members '[Input TopInfo, LFresh, Output (Message ())] r =>
  REPLExpr 'CShowDefn ->
  Sem r ()
handleShowDefn (ShowDefn x) = do
  let name2s = name2String x
  defns <- inputs @TopInfo (view (replModInfo . miTermdefs))
  tyDefns <- inputs @TopInfo (view (replModInfo . miTydefs))

  let xdefs = Ctx.lookupAll' (coerce x) defns
      mtydef = M.lookup name2s tyDefns

  info $ do
    let ds = map (pretty' . snd) xdefs ++ maybe [] (pure . pretty' . (name2s,)) mtydef
    case ds of
      [] -> text "No definition for" <+> pretty' x
      _nonEmptyList -> vcat ds

------------------------------------------------------------
-- :test

testPropCmd :: REPLCommand 'CTestProp
testPropCmd =
  REPLCommand
    { name = "test"
    , helpcmd = ":test <property>"
    , shortHelp = "Test a property using random examples"
    , category = User
    , cmdtype = ColonCmd
    , action = handleTest
    , parser = TestProp <$> term
    }

handleTest ::
  Members (Error DiscoError ': State TopInfo ': Output (Message ()) ': EvalEffects) r =>
  REPLExpr 'CTestProp ->
  Sem r ()
handleTest (TestProp t) = do
  at <- inputToState . typecheckTop $ checkProperty t
  tydefs <- use @TopInfo (replModInfo . to allTydefs)
  inputToState . inputTopEnv $ do
    r <- runTest 100 at -- XXX make configurable
    info $ runInputConst tydefs . runReader initPA $ indent 2 . nest 2 $ "-" <+> prettyTestResult at r

------------------------------------------------------------
-- :type

typeCheckCmd :: REPLCommand 'CTypeCheck
typeCheckCmd =
  REPLCommand
    { name = "type"
    , helpcmd = ":type <term>"
    , shortHelp = "Typecheck a term"
    , category = Dev
    , cmdtype = ColonCmd
    , action = inputToState @TopInfo . handleTypeCheck
    , parser = TypeCheck <$> parseTermOrOp
    }

maxInferredTypes :: Int
maxInferredTypes = 16

handleTypeCheck ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output (Message ())] r =>
  REPLExpr 'CTypeCheck ->
  Sem r ()
handleTypeCheck (TypeCheck t) = do
  asigs <- typecheckTop $ inferTop maxInferredTypes t
  sigs <- runFresh . mapInput (view (replModInfo . miTydefs)) $ thin $ NE.map snd asigs
  let (toShow, extra) = NE.splitAt 8 sigs
  when (length sigs > 1) $ info "This expression has multiple possible types.  Some examples:"
  info $
    vcat $
      map (\sig -> pretty' t <+> text ":" <+> pretty' sig) toShow
        ++ ["..." | not (P.null extra)]

------------------------------------------------------------

-- In :type, :doc, or :table commands, allow naked operators, as in :type + ,
-- even though + by itself is not a syntactically valid term.
-- However, this seems like it may be a common thing for a student to
-- ask and there is no reason we can't have this as a special case.
parseTermOrOp :: Parser Term
parseTermOrOp =
  (try term <?> "expression")
    <|> (parseNakedOp <?> "operator")

parseNakedOp :: Parser Term
parseNakedOp = TPrim <$> parseNakedOpPrim

parseNakedOpPrim :: Parser Prim
parseNakedOpPrim = sc *> choice (map mkOpParser (concat opTable))
 where
  mkOpParser :: OpInfo -> Parser Prim
  mkOpParser (OpInfo (UOpF _ op) syns _) = choice (map ((PrimUOp op <$) . reservedOp) syns)
  mkOpParser (OpInfo (BOpF _ op) syns _) = choice (map ((PrimBOp op <$) . reservedOp) syns)
