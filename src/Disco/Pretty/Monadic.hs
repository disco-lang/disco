{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Pretty
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Adapter DSL on top of Text.PrettyPrint for monadic pretty-printing.
--
-----------------------------------------------------------------------------

module Disco.Pretty.Monadic where

import           Control.Applicative              hiding (empty)
import           Prelude                          hiding ((<>))

import           Unbound.Generics.LocallyNameless (LFresh)

import           Capability.Reader
import           Capability.Source
import           Control.Monad.Reader             (ReaderT (..))

import           Text.PrettyPrint                 (Doc)
import qualified Text.PrettyPrint                 as PP

import           Disco.Pretty.Prec

------------------------------------------------------------
-- Adapter DSL
--
-- Each combinator here mirrors one from Text.PrettyPrint, but
-- operates over a generic functor/monad.

vcat :: Monad f => [f Doc] -> f Doc
vcat ds  = PP.vcat <$> sequence ds

hcat :: Monad f => [f Doc] -> f Doc
hcat ds  = PP.hcat <$> sequence ds

hsep :: Monad f => [f Doc] -> f Doc
hsep ds  = PP.hsep <$> sequence ds

parens :: Functor f => f Doc -> f Doc
parens   = fmap PP.parens

brackets :: Functor f => f Doc -> f Doc
brackets = fmap PP.brackets

braces :: Functor f => f Doc -> f Doc
braces = fmap PP.braces

bag :: Monad f => f Doc -> f Doc
bag p = text "⟅" <> p <> text "⟆"

quotes :: Functor f => f Doc -> f Doc
quotes = fmap PP.quotes

doubleQuotes :: Functor f => f Doc -> f Doc
doubleQuotes = fmap PP.doubleQuotes

text :: Monad m => String -> m Doc
text     = return . PP.text

integer :: Monad m => Integer -> m Doc
integer  = return . PP.integer

nest :: Functor f => Int -> f Doc -> f Doc
nest n d = PP.nest n <$> d

empty :: Monad m => m Doc
empty    = return PP.empty

(<+>) :: Applicative f => f Doc -> f Doc -> f Doc
(<+>) = liftA2 (PP.<+>)

(<>) :: Applicative f => f Doc -> f Doc -> f Doc
(<>)  = liftA2 (PP.<>)

($+$) :: Applicative f => f Doc -> f Doc -> f Doc
($+$) = liftA2 (PP.$+$)

punctuate :: Monad f => f Doc -> [f Doc] -> f [f Doc]
punctuate p ds = do
  p' <- p
  ds' <- sequence ds
  return . map return $ PP.punctuate p' ds'

------------------------------------------------------------
-- Concrete pretty-printing monad

-- This should probably be replaced at some point in the future, with
-- a single concrete application mega-monad.

newtype DocM m a = DocM { runDocM :: ReaderT PA m a }
  deriving (Functor, Applicative, Monad, LFresh)
  deriving (HasReader "pa" PA, HasSource "pa" PA) via
    MonadReader (ReaderT PA m)

type instance TypeOf _ "pa" = PA

renderDoc :: Functor m => DocM m Doc -> m String
renderDoc = fmap PP.render . flip runReaderT initPA . runDocM
