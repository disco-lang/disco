{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------

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
module Disco.Messages where

import Control.Lens
import Control.Monad (when)
import Polysemy
import Polysemy.Output

import Disco.Pretty (Doc, Pretty, pretty', renderDoc')

data MessageType
  = Info
  | Warning
  | ErrMsg
  | Debug
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Message = Message {_messageType :: MessageType, _message :: Doc}
  deriving (Show)

makeLenses ''Message

handleMsg :: Member (Embed IO) r => (Message -> Bool) -> Message -> Sem r ()
handleMsg p m = when (p m) $ printMsg m

printMsg :: Member (Embed IO) r => Message -> Sem r ()
printMsg (Message _ m) = embed $ putStrLn (renderDoc' m)

msg :: Member (Output Message) r => MessageType -> Sem r Doc -> Sem r ()
msg typ m = m >>= output . Message typ

info :: Member (Output Message) r => Sem r Doc -> Sem r ()
info = msg Info

infoPretty :: (Member (Output Message) r, Pretty t) => t -> Sem r ()
infoPretty = info . pretty'

warn :: Member (Output Message) r => Sem r Doc -> Sem r ()
warn = msg Warning

debug :: Member (Output Message) r => Sem r Doc -> Sem r ()
debug = msg Debug

debugPretty :: (Member (Output Message) r, Pretty t) => t -> Sem r ()
debugPretty = debug . pretty'

err :: Member (Output Message) r => Sem r Doc -> Sem r ()
err = msg ErrMsg
