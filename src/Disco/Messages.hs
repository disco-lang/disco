{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Messages
-- Copyright   :  (c) 2017 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Message logging framework (e.g. for errors, warnings, etc.) for
-- disco.
--
-----------------------------------------------------------------------------

module Disco.Messages where

import Unbound.Generics.LocallyNameless

import           Control.Monad.Writer
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Disco.AST.Surface
import Disco.AST.Typed
import Disco.AST.Core

data MessageLevel
  = Info
  | Warning
  | Error
  | Panic
  | Debug
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Report
  = RTxt   String
  | RName  AnyName
  | RTerm  Term
  | RPat   Pattern
  | RATerm ATerm
  | RCore  Core
  | RSeq   [Report]
  | RList  [Report]
  | RSub   Report
  deriving (Show)

data MessageBody e
  = Msg  Report
  | Item e
  deriving (Show, Functor)

data Message e = Message MessageLevel (MessageBody e)
  deriving (Show, Functor)

type MessageLog e = Seq (Message e)

emptyMessageLog :: MessageLog e
emptyMessageLog = Seq.empty
