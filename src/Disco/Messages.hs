{-# LANGUAGE DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Messages
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Message logging framework (e.g. for errors, warnings, etc.) for
-- disco.
--
-----------------------------------------------------------------------------

module Disco.Messages where


import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.AST.Typed

data MessageLevel
  = Info
  | Warning
  | Error
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
