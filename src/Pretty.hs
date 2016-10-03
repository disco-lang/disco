{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Pretty where

import           Prelude                 hiding (seq)

import           Control.Applicative     hiding (empty)
import           Control.Lens
import           Control.Monad.Reader
import           Data.Char               (toLower, isSpace)
import           Data.List               (findIndex, intersperse, transpose)
import           Data.List.Split         (splitOn)
import           Data.Maybe              (fromJust)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import           Data.Ratio

import qualified Parser                  as PR
import           Types

import qualified Text.PrettyPrint        as PP
import           Unbound.LocallyNameless (LFreshM, Name, lunbind, runLFreshM,
                                          unembed)

--------------------------------------------------
-- Internal document AST

data TextMode = ASCII | Unicode | LaTeX
  deriving (Eq, Show)

data Formatted where
  -- Terminals
  FEmpty    :: Formatted                 -- ^ The empty document
  FIdent    :: String    -> Formatted    -- ^ An identifier
  FKeyword  :: String    -> Formatted    -- ^ A keyword
  FType     :: String    -> Formatted    -- ^ A type name
  FSymbol   :: String    -> Formatted    -- ^ A symbol
  FInt      :: Integer   -> Formatted    -- ^ An integer
  FRat      :: Rational  -> Formatted    -- ^ A rational number
  FUnit     :: Formatted                 -- ^ The unit value/pattern

  FComma    :: Formatted                 -- ^ A comma
  FSpace    :: Formatted                 -- ^ Whitespace
  FNewline  :: Formatted                 -- ^ Move to a new line

  -- Structure
  FBlock    :: [Formatted] -> Formatted  -- ^ A vertically aligned
                                         --   block
  FAlign    :: Formatted                 -- ^ Alignment points get vertically aligned
                                         --   across lines, and scope within Blocks
  FIndent   :: Formatted   -> Formatted  -- ^ An indented section

  FAlt      :: [(TextMode, Formatted)] -> Formatted
                                         -- ^ Alternatives, depending on output mode
  FParens   :: Formatted -> Formatted    -- ^ Parenthesized expression
  FSequence :: [Formatted] -> Formatted  -- ^ Sequence of formatted expressions
  FCase     :: [Formatted] -> Formatted  -- ^ Case expression (a special kind of Block)
  FSuper    :: Formatted   -> Formatted  -- ^ Superscript
  FSub      :: Formatted   -> Formatted  -- ^ Subscript
  deriving (Eq, Show)

--------------------------------------------------
-- Precedence and associativity

type Prec = Int
data Assoc = AL | AR | AN
  deriving (Show, Eq)

prec :: BOp -> Prec
prec op = fromJust . findIndex (op `elem`) $
  [ []
  , []
  , [ Or ]
  , [ And ]
  , [ Eq, Neq, Lt, Gt, Leq, Geq, Divides, RelPm ]
  , []
  , [ Add, Sub ]
  , [ Mul, Div, Mod ]
  , [ Exp ]
  ]

assoc :: BOp -> Assoc
assoc op
  | op `elem` [Add, Sub, Mul, Div, Mod] = AL
  | op `elem` [And, Or, Exp]            = AR
  | otherwise                           = AN

getPA :: BOp -> PA
getPA op = PA (prec op) (assoc op)

data PA = PA Prec Assoc
  deriving (Show, Eq)

instance Ord PA where
  compare (PA p1 a1) (PA p2 a2) = compare p1 p2 `mappend` (if a1 == a2 then EQ else LT)

initPA :: PA
initPA = PA 0 AL

funPA :: PA
funPA = PA 10 AL

arrPA :: PA
arrPA = PA 1 AR

--------------------------------------------------
-- The formatting monad

data FormatCtx
  = FormatCtx
    { _formatPA        :: PA
    , _formatRightmost :: Bool
    }

makeLenses ''FormatCtx

initFormatCtx :: FormatCtx
initFormatCtx = FormatCtx initPA True

type MFormat = ReaderT FormatCtx LFreshM Formatted

format :: MFormat -> Formatted
format = runLFreshM . flip runReaderT initFormatCtx

alt :: [(TextMode, MFormat)] -> MFormat
alt fs = FAlt <$> sequence (map strength fs)
  where
    strength (a,b) = (a,) <$> b

ident :: String -> MFormat
ident = return . FIdent

keyword :: String -> MFormat
keyword = return . FKeyword

seq :: [MFormat] -> MFormat
seq fs = FSequence <$> sequence fs

seq' :: [MFormat] -> MFormat
seq' = seq . intersperse (return FSpace)

symbol :: String -> MFormat
symbol = return . FSymbol

ascii :: String -> (TextMode, MFormat)
ascii = (ASCII,) . symbol

unicode :: String -> (TextMode, MFormat)
unicode = (Unicode,) . symbol

tex :: String -> (TextMode, MFormat)
tex = (LaTeX,) . symbol

typeName :: String -> (TextMode, MFormat)
typeName = (ASCII,) . return . FType

comma :: Monad m => m Formatted
comma = return FComma

space :: Monad m => m Formatted
space = return FSpace

align :: Monad m => m Formatted
align = return FAlign

newline :: Monad m => m Formatted
newline = return FNewline

parens :: Functor f => f Formatted -> f Formatted
parens = fmap FParens

mparens :: PA -> MFormat -> MFormat
mparens pa fmt = do
  parentPA <- view formatPA
  (if (pa < parentPA) then parens else id) fmt

------------------------------------------------------------
-- Formatting syntax

formatTy :: Type -> MFormat
formatTy TyVoid          = alt [typeName "Void", tex "\\mathbbold{0}"]
formatTy TyUnit          = alt [typeName "Unit", tex "\\mathbbold{1}"]
formatTy TyBool          = alt [typeName "Bool", tex "\\mathbb{B}" ]
formatTy TyN             = alt [typeName "Nat",  tex "\\mathbb{N}" ]
formatTy TyZ             = alt [typeName "Int",  tex "\\mathbb{Z}" ]
formatTy TyQ             = alt [typeName "Rat",  tex "\\mathbb{Q}" ]
formatTy (TyArr τ₁ τ₂)   = mparens arrPA $
  seq'
  [ formatTy' 1 AL τ₁
  , alt [ascii "->", unicode "→", tex "\\to"]
  , formatTy' 1 AR τ₂
  ]
formatTy (TyPair τ₁ τ₂)  = mparens (PA 7 AR) $
  seq'
  [ formatTy' 7 AL τ₁
  , alt [ascii "*", unicode "×", tex "\\times"]
  , formatTy' 7 AR τ₂
  ]
formatTy (TySum τ₁ τ₂)   = mparens (PA 6 AR) $
  seq'
  [ formatTy' 6 AL τ₁
  , alt [ascii "+"]
  , formatTy' 6 AR τ₂
  ]

formatTy' :: Prec -> Assoc -> Type -> MFormat
formatTy' p a τ
  = local ((formatPA .~ PA p a) . (formatRightmost &&~ (a == AR)))
  $ formatTy τ

formatName :: Name Term -> MFormat
formatName = ident . show

formatTerm :: Term -> MFormat
formatTerm (TVar x)      = formatName x
formatTerm TUnit         = return FUnit
formatTerm (TBool b)     = keyword (map toLower $ show b)
formatTerm (TAbs bnd)    = mparens initPA $
   lunbind bnd $ \(x,body) ->
   seq'
     [ formatName x
     , alt
       [ ascii "->"
       , unicode "↦"
       , tex "\\mapsto"
       ]
     , formatTerm' 0 AL body
     ]
formatTerm (TJuxt t1 t2) = mparens funPA $
  seq' [formatTerm' 10 AL t1, formatTerm' 10 AR t2]
formatTerm (TPair t1 t2) =
  parens $ seq [ formatTerm' 0 AL t1, comma, space, formatTerm' 0 AL t2 ]
formatTerm (TInj side t) = mparens funPA $
  seq' [formatSide side, formatTerm' 10 AR t]
formatTerm (TNat n)      = return $ FInt n
formatTerm (TUn op t)    = seq [formatUOp op, formatTerm' 11 AL t]
  -- Need a special case for exponentiation since it may be typeset as
  -- a superscript
formatTerm (TBin Exp t1 t2) = mparens (getPA Exp) $
  seq
  [ formatTerm' (prec Exp) AL t1
  , alt
    [ (ASCII, seq [space, formatBOp Exp, space, formatTerm' (prec Exp) AR t2])
    , (LaTeX, FSuper <$> formatTerm' 0 AR t2)
    ]
  ]
formatTerm (TBin op t1 t2) = mparens (getPA op) $
  seq'
  [ formatTerm' (prec op) AL t1
  , formatBOp op
  , formatTerm' (prec op) AR t2
  ]
formatTerm (TLet bnd) = mparens initPA $
  lunbind bnd $ \((x, unembed -> t1), t2) ->
  seq'
    [ keyword "let"
    , formatName x
    , symbol "="
    , formatTerm' 0 AL t1
    , keyword "in"
    , formatTerm' 0 AL t2
    ]
formatTerm (TCase b)    = do
  rm <- view formatRightmost
  (if rm then id else parens) $
    FCase <$> mapM formatBranch b

  -- XXX FIX ME: what is the precedence of ascription?
formatTerm (TAscr t ty) =
  parens $ seq' [formatTerm t, symbol ":", formatTy ty]

formatTerm' :: Prec -> Assoc -> Term -> MFormat
formatTerm' p a t
  = local ((formatPA .~ PA p a) . (formatRightmost &&~ (a == AR)))
  $ formatTerm t

formatSide :: Side -> MFormat
formatSide L = keyword "inl"
formatSide R = keyword "inr"

formatUOp :: UOp -> MFormat
formatUOp Neg = symbol "-"
formatUOp Not =
  alt
  [ (ASCII, seq [keyword "not", space])
  , unicode "¬"
  , tex "\\neg"
  ]

formatBOp :: BOp -> MFormat
formatBOp Add     = symbol "+"
formatBOp Sub     = alt [ascii "-", unicode "−"]
formatBOp Mul     = alt [ascii "*", unicode "×", tex "\\times"]
formatBOp Div     = alt [ascii "/", unicode "∕"]
formatBOp Exp     = symbol "^"
formatBOp Eq      = alt [ascii "==", unicode "≡", tex "\\equiv"]
formatBOp Neq     = alt [ascii "/=", unicode "≠", tex "\\neq"]
formatBOp Lt      = symbol "<"
formatBOp Gt      = symbol ">"
formatBOp Leq     = alt [ascii "<=", unicode "≤", tex "\\leq"]
formatBOp Geq     = alt [ascii ">=", unicode "≥", tex "\\geq"]
formatBOp And     = alt [(ASCII, keyword "and"), unicode "∧", tex "\\wedge"]
formatBOp Or      = alt [(ASCII, keyword "or"), unicode "∨", tex "\\vee"]
formatBOp Mod     = alt [(ASCII, keyword "mod")] -- XXX what to do for TeX?
formatBOp Divides = alt [ascii "|", tex "\\mid"]
formatBOp RelPm   = alt [ascii "#", tex "\\#"]

formatBranch :: Branch -> MFormat
formatBranch br =
  lunbind br $ \(gs,t) ->
  seq [formatTerm t, formatGuards gs]

formatGuards :: [Guard] -> MFormat
formatGuards [] = seq [align, keyword "otherwise"]
formatGuards gs = seq $ intersperse newline (map formatGuard gs)

formatGuard :: Guard -> MFormat
formatGuard (GIf et) =
  seq [ align, keyword "if", align, formatTerm (unembed et)]
formatGuard (GWhen et p) =
  seq [ align, keyword "when", align
      , seq' [formatTerm (unembed et), symbol "=", formatPattern p]
      ]

formatPattern :: Pattern -> MFormat
formatPattern (PVar x)      = formatName x
formatPattern PWild         = alt [ascii "_", tex "\\_"]
formatPattern PUnit         = return FUnit
formatPattern (PBool b)     = keyword (map toLower $ show b)
formatPattern (PPair p1 p2) = parens $ seq [formatPattern p1, comma, space, formatPattern p2]
formatPattern (PInj s p)    = seq' [formatSide s, formatPattern p]
formatPattern (PNat n)      = return $ FInt n
formatPattern (PSucc p)     = seq' [keyword "S", formatPattern p]
  -- XXX todo need parens around succ if it's inside e.g. inl

------------------------------------------------------------
-- Rendering to String

--------------------------------------------------
-- String boxes

newtype Box = Box [String]

instance Show Box where
  show (Box ss) = unlines ss

str :: String -> Box
str s = Box [s]

-- | "Fuse" two boxes horizontally, by aligning their tops and
--   zipping.  The result will be truncated to the height of the
--   smaller box.
hfuse :: Box -> Box -> Box
hfuse (Box b1) (Box b2) = Box (zipWith (<>) b1 b2)

-- | "Glue" two boxes together, placing the first line of the second
--   box after the last line of the first.  The height of the result
--   will be one less than the sum of the heights.
hglue :: Box -> Box -> Box
hglue (Box b1) (Box b2) =
  (Box b1 `vglue` spacer len1)
  `hfuse`
  ((Box (replicate (length b1 - 1) [])) `vglue` (Box b2))
  where
    len1 = length (last b1)

-- | Horizontally concatenate a list of boxes, via gluing.
hcat :: [Box] -> Box
hcat = foldr hglue (Box [""])

vglue :: Box -> Box -> Box
vglue (Box b1) (Box b2) = Box (b1 <> b2)

vcat :: [Box] -> Box
vcat = foldr vglue (Box [])

pad :: Box -> Box
pad (Box ls) = Box (map padStr ls)
  where
    len      = maximum (0 : map length ls)
    padStr s = s ++ replicate (len - length s) ' '

-- | A box which is n spaces wide and infinitely tall.  Useful in
--   the context of 'hcat'.
spacer :: Int -> Box
spacer n = Box (repeat (replicate n ' '))

--------------------------------------------------
-- Rendering to Boxes

data RenderCtx =
  RenderCtx
  { _textMode  :: TextMode
  }

initRenderCtx :: RenderCtx
initRenderCtx = RenderCtx { _textMode = ASCII }

makeLenses ''RenderCtx

type RenderM = Reader RenderCtx

runRenderM :: RenderM a -> a
runRenderM = flip runReader initRenderCtx

leaf :: String -> RenderM Box
leaf s = return $ str s

concatBoxes :: [RenderM Box] -> RenderM Box
concatBoxes ss = hcat <$> sequence ss

renderBox :: Formatted -> RenderM Box
renderBox FEmpty           = leaf ""
renderBox (FAlt [])        = leaf ""
renderBox (FAlt fs)        = do
  mode <- view textMode
  renderBox $ fromMaybe (snd . head $ fs) (lookup mode fs)
renderBox (FIdent s)       = leaf s
renderBox (FKeyword s)     = leaf s
renderBox (FType s)        = leaf s
renderBox (FSymbol s)      = leaf s
renderBox (FInt i)         = leaf $ show i
renderBox (FRat r)
  | denominator r == 1     = leaf $ show (numerator r)
  | otherwise              = leaf $ show (numerator r) ++ "/" ++ show (denominator r)
renderBox FUnit            = leaf "()"
renderBox FComma           = leaf ","
renderBox FSpace           = leaf " "
renderBox (FIndent f)      = (spacer 2 `hfuse`) <$> renderBox f
renderBox (FBlock f)       = renderBlock f
renderBox (FParens f)      = concatBoxes [leaf "(", renderBox f, leaf ")"]
renderBox (FSequence fs)   = renderSequence fs
renderBox (FCase fs)       = ((Box (repeat "{ ")) `hfuse`) <$> renderBox (FBlock fs)

renderBox FAlign           = leaf ""
    -- ignore FAlign if it occurs outside an FBlock
renderBox FNewline
  = error "renderBox FNewline: this should never happen!"
    -- case should never happen; should be handled in FSequence case

renderBox (FSub f)         = renderBox f
renderBox (FSuper f)       = renderBox f
    -- ignore sub- and super-scripts in text generation mode

renderBlock :: [Formatted] -> RenderM Box
renderBlock
  = fmap (foldr hfuse (spacer 0) . intersperse (spacer 1)
          . map (pad . vcat) . transpose)
  . mapM (mapM renderSequence)
  . map (splitOn [FAlign])
  . concat . map (splitOn [FNewline])
  . map flatten

flatten :: Formatted -> [Formatted]
flatten (FSequence fs) = concatMap flatten fs
flatten f              = [f]

renderSequence :: [Formatted] -> RenderM Box
renderSequence
  = (vcat <$>)
  . mapM (concatBoxes . map renderBox)
  . splitOn [FNewline]

--------------------------------------------------
-- Monadic pretty-printing

-- hsep :: Monad f => [f PP.Doc] -> f PP.Doc
-- hsep ds  = PP.hsep <$> sequence ds

-- parens :: Functor f => f PP.Doc -> f PP.Doc
-- parens   = fmap PP.parens

-- text :: Monad m => String -> m PP.Doc
-- text     = return . PP.text

-- integer :: Monad m => Integer -> m PP.Doc
-- integer  = return . PP.integer

-- nest :: Functor f => Int -> f PP.Doc -> f PP.Doc
-- nest n d = PP.nest n <$> d

-- empty :: Monad m => m PP.Doc
-- empty    = return PP.empty

-- (<+>) :: Applicative f => f PP.Doc -> f PP.Doc -> f PP.Doc
-- (<+>) = liftA2 (PP.<+>)

-- (<>) :: Applicative f => f PP.Doc -> f PP.Doc -> f PP.Doc
-- (<>)  = liftA2 (PP.<>)

-- ($+$) :: Applicative f => f PP.Doc -> f PP.Doc -> f PP.Doc
-- ($+$) = liftA2 (PP.$+$)

-- ------------------------------------------------------------

-- prettyProg :: Prog -> Doc
-- prettyProg = foldr ($+$) empty . map prettyDecl

-- prettyDecl :: Decl -> Doc
-- prettyDecl (DType x ty) = prettyName x <+> text ":" <+> prettyTy ty
-- prettyDecl (DDefn x b)
--   = lunbind b $ \(ps, t) ->
--   (prettyName x <+> (hsep $ map prettyPattern ps) <+> text "=" <+> prettyTerm t) $+$ text " "

-- ------------------------------------------------------------

-- renderDoc :: Doc -> String
-- renderDoc = PP.render . runLFreshM . flip runReaderT initPA

-- echoTerm :: String -> String
-- echoTerm = renderDoc . prettyTerm . PR.parseTermStr

-- echoTermP :: String -> IO ()
-- echoTermP = putStrLn . echoTerm

-- echoType :: String -> String
-- echoType = renderDoc . prettyTy . PR.parseTypeStr
