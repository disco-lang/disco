
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.MessageLog
-- Copyright   :  (c) 2017 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Utilities for dealing with the message log stored by the 'Disco'
-- monad.
--
-----------------------------------------------------------------------------

module Disco.MessageLog
  (
    -- * Generating messages

    emitMessage

    -- $msg
  , info, warning, err, panic, debug

    -- $msgR
  , infoR, warningR, errR, panicR, debugR

    -- * Buffering
  , disableMessageBuffering

    -- * Turning exceptions into messages
  , catchMessage

    -- * Formatting messages
  , printAndClearMessages, printMessages
  , formatMessages, formatMessage
  ) where

import           Control.Lens  (use, (.=), (<>=))
import           Control.Monad ((>=>), when)
import           Data.Coerce   (coerce)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import           Data.Void

import           Unbound.Generics.LocallyNameless

import           Disco.Messages
import           Disco.Eval
import           Disco.Pretty
import           Disco.Typecheck (erase)

-- | Emit a message of the given severity level, either by appending
--   it to the message log, or immediately outputting it, depending on
--   the current 'bufferMessages' setting.
emitMessage :: MessageLevel -> Report -> Disco e ()
emitMessage lev body = do
  let msg = Message lev body
  buf <- use bufferMessages
  case buf of
    True  -> messageLog <>= Seq.singleton msg
    False -> do
      d <- renderDoc $ formatMessage msg
      iputStrLn d

-- $msg
-- Convenient functions for generating a message of a given
-- severity level from a single @String@.

info, warning, err, panic, debug :: String -> Disco e ()
info     = infoR    . RTxt
warning  = warningR . RTxt
err      = errR     . RTxt
panic    = panicR   . RTxt
debug    = debugR   . RTxt

-- $msgR
-- Convenient functions for generating a message of a given
-- severity level from a 'Report'.

infoR, warningR, errR, panicR, debugR :: Report -> Disco e ()
infoR    = emitMessage Info
warningR = emitMessage Warning
errR     = emitMessage Error
panicR   = emitMessage Panic
debugR   = emitMessage Debug

-- | Locally disable message buffering within the given @Disco@
--   computation.
disableMessageBuffering :: Disco e a -> Disco e a
disableMessageBuffering m = do
  b <- use bufferMessages
  bufferMessages .= False
  a <- m
  bufferMessages .= b
  return a

-- | Run a @Disco@ computation; if it throws an exception, catch it
--   and turn it into an error message using the given rendering
--   function, and also return it.  This is like 'catchEither', but
--   also adds the rendered error to the message log. The resulting
--   computation is statically guaranteed to throw no exceptions.
catchMessage :: (e -> Disco Void Report) -> Disco e a -> Disco void (Either e a)
catchMessage render m = do
  res <- catchEither m
  either ((noErrors . render) >=> errR) (const $ return ()) res

  return res

-- | Print all the messages in the message log to the console, and
--   also delete them from the log.  Hence this action is idempotent,
--   i.e. @printAndClearMessages >> printAndClearMessages =
--   printAndClearMessages@.
printAndClearMessages :: Disco void ()
printAndClearMessages = do
  printMessages
  messageLog .= Seq.empty

-- | Print all the messages in the message log to the console, but do
--   not remove them from the message log.  Hence calling this twice
--   will print all the messages twice.
printMessages :: Disco void ()
printMessages = do
  msgs <- use messageLog
  when (not $ F.null msgs) $ do
    let f = formatMessages (F.toList msgs)
    s    <- renderDoc f
    iputStrLn s

-- | Pretty-print a list of messages.
formatMessages :: [Message] -> Doc
formatMessages = vcat . map formatMessage

-- | Pretty-print a single message.
formatMessage :: Message -> Doc
formatMessage (Message lvl (RTxt s)) = text (formatLevel lvl <:> s)
  where
    (<:>) ""  = id
    (<:>) lbl = ((lbl ++ ": ") ++)
formatMessage (Message _ rpt) = formatReport rpt

-- | Pretty-print a @MessageLevel@.
formatLevel :: MessageLevel -> String
formatLevel Info    = ""
formatLevel Warning = "Warning"
formatLevel Error   = "Error"
formatLevel Panic   = "Panic!"
formatLevel Debug   = "DEBUG"


-- XXX take the current indent level into account, once it is added.

-- | Pretty-print a 'Report'.
formatReport :: Report -> Doc
formatReport (RTxt s)            = text s
formatReport (RName (AnyName x)) = prettyName $ coerce x
formatReport (RTerm t)           = prettyTerm t
formatReport (RPat p)            = prettyPattern p
formatReport (RATerm at)         = prettyTerm (erase at)
formatReport (RCore c)           = text $ show c
formatReport (RSeq rs)           = hcat . map formatReport $ rs
formatReport (RList rs)          = vcat . map formatReport $ rs
formatReport (RSub r)            = nest 2 $ formatReport r
