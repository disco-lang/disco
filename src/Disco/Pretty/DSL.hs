-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Pretty.DSL
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Adapter DSL on top of Text.PrettyPrint for Applicative pretty-printing.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Disco.Pretty.DSL where

import           Control.Applicative hiding (empty)
import           Data.String         (IsString (..))
import           Prelude             hiding ((<>))

import           Polysemy
import           Polysemy.Reader

import           Text.PrettyPrint    (Doc)
import qualified Text.PrettyPrint    as PP

import           Disco.Pretty.Prec

instance IsString (Sem r Doc) where
  fromString = text

------------------------------------------------------------
-- Adapter DSL
--
-- Each combinator here mirrors one from Text.PrettyPrint, but
-- operates over a generic functor/monad.

vcat :: Applicative f => [f Doc] -> f Doc
vcat ds  = PP.vcat <$> sequenceA ds

hcat :: Applicative f => [f Doc] -> f Doc
hcat ds  = PP.hcat <$> sequenceA ds

hsep :: Applicative f => [f Doc] -> f Doc
hsep ds  = PP.hsep <$> sequenceA ds

parens :: Functor f => f Doc -> f Doc
parens   = fmap PP.parens

brackets :: Functor f => f Doc -> f Doc
brackets = fmap PP.brackets

braces :: Functor f => f Doc -> f Doc
braces = fmap PP.braces

bag :: Applicative f => f Doc -> f Doc
bag p = text "⟅" <> p <> text "⟆"

quotes :: Functor f => f Doc -> f Doc
quotes = fmap PP.quotes

doubleQuotes :: Functor f => f Doc -> f Doc
doubleQuotes = fmap PP.doubleQuotes

text :: Applicative m => String -> m Doc
text     = pure . PP.text

integer :: Applicative m => Integer -> m Doc
integer  = pure . PP.integer

nest :: Functor f => Int -> f Doc -> f Doc
nest n d = PP.nest n <$> d

hang :: Applicative f => f Doc -> Int -> f Doc -> f Doc
hang d1 n d2 = PP.hang <$> d1 <*> pure n <*> d2

empty :: Applicative m => m Doc
empty    = pure PP.empty

(<+>) :: Applicative f => f Doc -> f Doc -> f Doc
(<+>) = liftA2 (PP.<+>)

(<>) :: Applicative f => f Doc -> f Doc -> f Doc
(<>)  = liftA2 (PP.<>)

($+$) :: Applicative f => f Doc -> f Doc -> f Doc
($+$) = liftA2 (PP.$+$)

punctuate :: Applicative f => f Doc -> [f Doc] -> f [f Doc]
punctuate p ds = map pure <$> (PP.punctuate <$> p <*> sequenceA ds)

intercalate :: Monad f => f Doc -> [f Doc] -> f Doc
intercalate p ds = do
  ds' <- punctuate p ds
  hsep ds'

bulletList :: Applicative f => f Doc -> [f Doc] -> f Doc
bulletList bullet = vcat . map (hang bullet 2)

------------------------------------------------------------
-- Running a pretty-printer

renderDoc :: Sem (Reader PA ': r) Doc -> Sem r String
renderDoc = fmap PP.render . runReader initPA

renderDoc' :: Doc -> String
renderDoc' = PP.render
