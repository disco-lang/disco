-- Version 1, 18 Oct 2016

-- This is just like Parsing, but re-exports the general versions of
-- operators like (<*>) and so on from the Prelude, instead of
-- Parser-specific versions.

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parsing2
  ( -- * Lexing

    TokenParser, makeTokenParser, emptyDef, GenLanguageDef(..)

  , getIdentifier, getReserved, getReservedOp
  , getNatural, getInteger, getFloat, getNaturalOrFloat
  , getSymbol
  , getWhiteSpace, getParens, getAngles

    -- * Parsing

  , Parser, parse, parseFile, parseSome
  , (<|>), (<$>), (<*>), (<$), (<*), (*>)
  , module Text.Parsec.Expr
  , module Text.Parsec.Combinator
  , module Text.Parsec.Char
  , try
  )
  where

import qualified Text.Parsec            as P
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr       hiding (Operator)
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.Pos
import           Text.Parsec.Prim       (Consumed (..), Reply (..), State (..),
                                         runParsecT, try)
import           Text.Parsec.String     (Parser, parseFromFile)
import           Text.Parsec.Token

import           Control.Applicative
import           Data.Functor.Identity

------------------------------------------------------------
-- Lexing
------------------------------------------------------------

getIdentifier     = identifier
getReserved       = reserved
getReservedOp     = reservedOp
getNatural        = natural
getInteger        = integer
getFloat          = float
getNaturalOrFloat = naturalOrFloat
getSymbol         = symbol
getWhiteSpace     = whiteSpace
getParens         = parens
getAngles         = angles

-- For more, see http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Token.html

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

parse :: Parser a -> String -> Either P.ParseError a
parse p = P.parse p ""

parseFile :: Parser a -> FilePath -> IO (Either P.ParseError a)
parseFile = parseFromFile

parseSome :: Parser a -> String -> Either P.ParseError (a, String)
parseSome p s =
  case runIdentity . getReply . runIdentity $ runParsecT p (State s (initialPos "") ()) of
    Ok a (State rest _ _) _ -> Right (a, rest)
    Error err               -> Left err
  where
    getReply (Consumed r) = r
    getReply (Empty    r) = r
