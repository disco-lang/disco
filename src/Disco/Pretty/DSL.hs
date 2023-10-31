{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Disco.Pretty.DSL
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Adapter DSL on top of Text.PrettyPrint for Applicative pretty-printing.
module Disco.Pretty.DSL where

import Control.Applicative hiding (empty)
import Data.String (IsString (..))
import Prelude hiding ((<>))
import Polysemy
import Polysemy.Reader
import Prettyprinter (Doc)
import Prettyprinter.Render.String (renderString)
import qualified Prettyprinter as PP
import Disco.Pretty.Prec

instance IsString (Sem r (Doc ann)) where
  fromString = text

------------------------------------------------------------
-- Adapter DSL
--
-- Each combinator here mirrors one from Text.PrettyPrint, but
-- operates over a generic functor/monad.

vcat :: Applicative f => [f (Doc ann)] -> f (Doc ann)
vcat ds = PP.vcat <$> sequenceA ds

hcat :: Applicative f => [f (Doc ann)] -> f (Doc ann)
hcat ds = PP.hcat <$> sequenceA ds

hsep :: Applicative f => [f (Doc ann)] -> f (Doc ann)
hsep ds = PP.hsep <$> sequenceA ds

parens :: Functor f => f (Doc ann) -> f (Doc ann)
parens = fmap PP.parens

brackets :: Functor f => f (Doc ann) -> f (Doc ann)
brackets = fmap PP.brackets

braces :: Functor f => f (Doc ann) -> f (Doc ann)
braces = fmap PP.braces

bag :: Applicative f => f (Doc ann) -> f (Doc ann)
bag p = text "⟅" <> p <> text "⟆"

quotes :: Functor f => f (Doc ann) -> f (Doc ann)
quotes = fmap PP.squotes

doubleQuotes :: Functor f => f (Doc ann) -> f (Doc ann)
doubleQuotes = fmap PP.dquotes

text :: Applicative m => String -> m (Doc ann)
text = pure . fromString

integer :: Applicative m => Integer -> m (Doc ann)
integer = pure . PP.pretty

nest :: Functor f => Int -> f (Doc ann) -> f (Doc ann)
nest n d = PP.nest n <$> d

hang :: Applicative f => f (Doc ann) -> Int -> f (Doc ann) -> f (Doc ann)
hang d1 n d2 = d1 <+> nest n d2

empty :: Applicative m => m (Doc ann)
empty = pure PP.emptyDoc

(<+>) :: Applicative f => f (Doc ann) -> f (Doc ann) -> f (Doc ann)
(<+>) = liftA2 (PP.<+>)

(<>) :: Applicative f => f (Doc ann) -> f (Doc ann) -> f (Doc ann)
(<>) = liftA2 (PP.<>)

($+$) :: Applicative f => f (Doc ann) -> f (Doc ann) -> f (Doc ann)
d1 $+$ d2 = PP.vcat <$> sequenceA [d1,d2]

punctuate :: Applicative f => f (Doc ann) -> [f (Doc ann)] -> f [f (Doc ann)]
punctuate p ds = map pure <$> (PP.punctuate <$> p <*> sequenceA ds)

intercalate :: Monad f => f (Doc ann) -> [f (Doc ann)] -> f (Doc ann)
intercalate p ds = do
  ds' <- punctuate p ds
  hsep ds'

bulletList :: Applicative f => f (Doc ann) -> [f (Doc ann)] -> f (Doc ann)
bulletList bullet = vcat . map (hang bullet 2)

------------------------------------------------------------
-- Running a pretty-printer

renderDoc :: Sem (Reader PA ': r) (Doc ann) -> Sem r String
renderDoc = fmap renderDoc' . runReader initPA

renderDoc' :: Doc ann -> String
renderDoc' = renderString . PP.layoutPretty PP.defaultLayoutOptions
