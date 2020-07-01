-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.CmdLine
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Expression type and parser for things entered at an interactive disco
-- prompt.
--
-----------------------------------------------------------------------------

module Disco.Interactive.CmdLine
  ( -- * Command-line options record

    DiscoOpts(..)

    -- * optparse-applicative command line parsers
  , discoOpts, discoInfo

    -- * main

  , discoMain

  ) where

import           Control.Monad             (when)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.List                 (isPrefixOf)
import           Data.Maybe                (isJust)
import           System.Exit               (exitFailure, exitSuccess)

import qualified Options.Applicative       as O
import           System.Console.Haskeline  as H

import           Disco.Eval
import           Disco.Interactive.Eval
import           Disco.Interactive.Commands (handleLoad, loadFile)

------------------------------------------------------------
-- Command-line options parser
------------------------------------------------------------

-- | Command-line options for disco.
data DiscoOpts = DiscoOpts
  { evaluate  :: Maybe String  -- ^ A single expression to evaluate
  , cmdFile   :: Maybe String  -- ^ Execute the commands in a given file
  , checkFile :: Maybe String  -- ^ Check a file and then exit
  }

discoOpts :: O.Parser DiscoOpts
discoOpts = DiscoOpts
  <$> O.optional (
        O.strOption (mconcat
          [ O.long "evaluate"
          , O.short 'e'
          , O.help "evaluate an expression"
          , O.metavar "TERM"
          ])
      )
  <*> O.optional (
        O.strOption (mconcat
          [ O.long "file"
          , O.short 'f'
          , O.help "execute the commands in a file"
          , O.metavar "FILE"
          ])
      )
  <*> O.optional (
        O.strOption (mconcat
          [ O.long "check"
          , O.help "check a file without starting the interactive REPL"
          , O.metavar "FILE"
          ])
      )

discoInfo :: O.ParserInfo DiscoOpts
discoInfo = O.info (O.helper <*> discoOpts) $ mconcat
  [ O.fullDesc
  , O.progDesc "Command-line interface for Disco, a programming language for discrete mathematics."
  , O.header "disco v0.1"
  ]

------------------------------------------------------------
-- Command-line interface
------------------------------------------------------------

banner :: String
banner = "Welcome to Disco!\n\nA language for programming discrete mathematics.\n\n"

discoMain :: IO ()
discoMain = do
  opts <- O.execParser discoInfo

  let batch = any isJust [evaluate opts, cmdFile opts, checkFile opts]
      settings = defaultSettings
            { historyFile = Just ".disco_history" }
  when (not batch) $ putStr banner
  res <- runDisco $ do
    case checkFile opts of
      Just file -> do
        res <- handleLoad file
        io $ if res then exitSuccess else exitFailure
      Nothing   -> return ()
    case cmdFile opts of
      Just file -> do
        mcmds <- loadFile file
        case mcmds of
          Nothing   -> return ()
          Just cmds -> mapM_ handleCMD (lines cmds)
      Nothing   -> return ()
    case evaluate opts of
      Just str -> handleCMD str
      Nothing  -> return ()

    when (not batch) $ runInputT settings loop

  case res of

    -- All disco exceptions should be caught and handled by this point.
    Left e   -> do
      putStrLn $ "Uncaught error: " ++ show e
      putStrLn $ "Please report this as a bug: https://github.com/disco-lang/disco/issues"
    Right () -> return ()

  -- XXX pretty-print log messages here

  where

    ctrlC :: InputT (Disco e) a -> SomeException -> InputT (Disco e) a
    ctrlC act e = do
      io $ putStrLn (show e)
      act

    withCtrlC resume act = H.catch (H.withInterrupt act) (ctrlC resume)

    loop :: InputT (Disco IErr) ()
    loop = do
      minput <- withCtrlC (return $ Just "") (getInputLine "Disco> ")
      case minput of
        Nothing -> return ()
        Just input
          | ":q" `isPrefixOf` input && input `isPrefixOf` ":quit" -> do
              liftIO $ putStrLn "Goodbye!"
              return ()
          | otherwise -> do
              withCtrlC (return ()) $ (lift . handleCMD $ input)
              loop
