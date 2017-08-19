
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.MessageLog
-- Copyright   :  (c) 2017 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- XXX
--
-----------------------------------------------------------------------------

module Disco.MessageLog
  (
    emitMessage, info, warning, err, panic, debug
  , infoR, warningR, errR, panicR, debugR

  , catchMessage

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

-- XXX comment everything!

emitMessage :: MessageLevel -> Report -> Disco e ()
emitMessage lev body = messageLog <>= Seq.singleton (Message lev body)

info, warning, err, panic, debug :: String -> Disco e ()
info     = infoR    . RTxt
warning  = warningR . RTxt
err      = errR     . RTxt
panic    = panicR   . RTxt
debug    = debugR   . RTxt

infoR, warningR, errR, panicR, debugR :: Report -> Disco e ()
infoR    = emitMessage Info
warningR = emitMessage Warning
errR     = emitMessage Error
panicR   = emitMessage Panic
debugR   = emitMessage Debug

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

printAndClearMessages :: Disco void ()
printAndClearMessages = do
  printMessages
  messageLog .= Seq.empty

-- XXX
printMessages :: Disco void ()
printMessages = do
  msgs <- use messageLog
  when (not $ F.null msgs) $ do
    let f = formatMessages (F.toList msgs)
    s    <- renderDoc f
    iputStrLn s

formatMessages :: [Message] -> Doc
formatMessages = vcat . map formatMessage

formatMessage :: Message -> Doc
formatMessage (Message lvl (RTxt s)) = text (formatLevel lvl <:> s)
  where
    (<:>) ""  = id
    (<:>) lbl = ((lbl ++ ": ") ++)
formatMessage (Message _ rpt) = formatReport rpt

formatLevel :: MessageLevel -> String
formatLevel Info    = ""
formatLevel Warning = "Warning"
formatLevel Error   = "Error"
formatLevel Panic   = "Panic!"
formatLevel Debug   = "DEBUG"

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
