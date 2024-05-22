{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

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

data Message ann = Message {_messageType :: MessageType, _message :: Doc ann}
  deriving (Show)

makeLenses ''Message

handleMsg :: Member (Embed IO) r => (Message ann -> Bool) -> Message ann -> Sem r ()
handleMsg p m = when (p m) $ printMsg m

printMsg :: Member (Embed IO) r => Message ann -> Sem r ()
printMsg (Message _ m) = embed $ putStrLn (renderDoc' m)

msg :: Member (Output (Message ann)) r => MessageType -> Sem r (Doc ann) -> Sem r ()
msg typ m = m >>= output . Message typ

info :: Member (Output (Message ann)) r => Sem r (Doc ann) -> Sem r ()
info = msg Info

infoPretty :: (Member (Output (Message ann)) r, Pretty t) => t -> Sem r ()
infoPretty = info . pretty'

warn :: Member (Output (Message ann)) r => Sem r (Doc ann) -> Sem r ()
warn = msg Warning

debug :: Member (Output (Message ann)) r => Sem r (Doc ann) -> Sem r ()
debug = msg Debug

debugPretty :: (Member (Output (Message ann)) r, Pretty t) => t -> Sem r ()
debugPretty = debug . pretty'

err :: Member (Output (Message ann)) r => Sem r (Doc ann) -> Sem r ()
err = msg ErrMsg
