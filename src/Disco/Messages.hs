{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- import           Unbound.Generics.LocallyNameless

-- import           Disco.AST.Core
-- import           Disco.AST.Surface
-- import           Disco.AST.Typed

import           Control.Lens
import           Control.Monad   (when)
import           Polysemy
import           Polysemy.Output

import           Disco.Pretty    (Pretty, prettyStr)

data MessageType
  = Info
  | Warning
  | Error
  | Debug
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- data Report
--   = RTxt   String
--   | RName  AnyName
--   | RTerm  Term
--   | RPat   Pattern
--   | RATerm ATerm
--   | RCore  Core
--   | RSeq   [Report]
--   | RList  [Report]
--   | RSub   Report
--   deriving (Show)

data Message = Message { _messageType :: MessageType, _message :: String }
  deriving (Show)

makeLenses ''Message

handleMsg :: Member (Embed IO) r => (Message -> Bool) -> Message -> Sem r ()
handleMsg p m = when (p m) $ printMsg m

printMsg :: Member (Embed IO) r => Message -> Sem r ()
printMsg (Message _ m)     = embed $ putStr m

msgLn :: Member (Output Message) r => MessageType -> String -> Sem r ()
msgLn typ = msg typ . (++"\n")

msg :: Member (Output Message) r => MessageType -> String -> Sem r ()
msg typ = output . Message typ

info :: Member (Output Message) r => String -> Sem r ()
info = msgLn Info

info' :: Member (Output Message) r => String -> Sem r ()
info' = msg Info

warn :: Member (Output Message) r => String -> Sem r ()
warn = msgLn Warning

debug :: Member (Output Message) r => String -> Sem r ()
debug = msgLn Debug

debug' :: Member (Output Message) r => String -> Sem r ()
debug' = msg Debug

debugPretty :: (Member (Output Message) r, Pretty t) => t -> Sem r ()
debugPretty t = debug =<< prettyStr t

err :: Member (Output Message) r => String -> Sem r ()
err = msgLn Error
