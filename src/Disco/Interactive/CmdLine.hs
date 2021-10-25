-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.CmdLine
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definition of the command-line REPL interface for Disco.
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

import           Control.Lens                           (view)
import           Control.Monad                          (unless, (>=>))
import qualified Control.Monad.Catch                    as CMC
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Foldable                          (forM_)
import           Data.List                              (isPrefixOf)
import           Data.Maybe                             (isJust)
import           System.Exit                            (exitFailure,
                                                         exitSuccess)

import qualified Options.Applicative                    as O
import           System.Console.Haskeline               as H

import           Disco.Error
import           Disco.Eval
import           Disco.Interactive.Commands
import           Disco.Module                           (LoadingMode (..),
                                                         Resolver (..))

import           Disco.Effects.Output
import           Polysemy
import           Polysemy.ConstraintAbsorber.MonadCatch
import           Polysemy.Error
import           Polysemy.State

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
  unless batch $ putStr banner
  runDisco $ do
    forM_ standardModules $ loadDiscoModule True FromStdlib >=> addModule Standalone
    case checkFile opts of
      Just file -> do
        res <- handleLoad file
        liftIO $ if res then exitSuccess else exitFailure
      Nothing   -> return ()
    case cmdFile opts of
      Just file -> do
        mcmds <- loadFile file
        case mcmds of
          Nothing   -> return ()
          Just cmds -> mapM_ handleCMD (lines cmds)
      Nothing   -> return ()
    forM_ (evaluate opts) handleCMD
    unless batch loop

  where

    -- These types used to involve InputT Disco, but we now use Final
    -- (InputT IO) in the list of effects.  see
    -- https://github.com/polysemy-research/polysemy/issues/395 for
    -- inspiration.

    ctrlC :: MonadIO m => m a -> SomeException -> m a
    ctrlC act e = do
      liftIO $ print e
      act

    withCtrlC :: (MonadIO m, CMC.MonadCatch m) => m a -> m a -> m a
    withCtrlC resume act = CMC.catch act (ctrlC resume)

    loop :: Members DiscoEffects r => Sem r ()
    loop = do
      minput <- embedFinal $ withCtrlC (return $ Just "") (getInputLine "Disco> ")
      case minput of
        Nothing -> return ()
        Just input
          | ":q" `isPrefixOf` input && input `isPrefixOf` ":quit" -> do
              liftIO $ putStrLn "Goodbye!"
              return ()
          | ":{" `isPrefixOf` input -> do
              multiLineLoop []
              loop
          | otherwise -> do
              mapError @_ @DiscoError (Panic . show) $
                absorbMonadCatch $
                withCtrlC (return ()) $
                handleCMD input
              loop

    multiLineLoop :: Members DiscoEffects r => [String] -> Sem r ()
    multiLineLoop ls = do
      minput <- embedFinal $ withCtrlC (return Nothing) (getInputLine "Disco| ")
      case minput of
        Nothing -> return ()
        Just input
          | ":}" `isPrefixOf` input -> do
              mapError @_ @DiscoError (Panic . show) $
                absorbMonadCatch $
                withCtrlC (return ()) $
                handleCMD (unlines (reverse ls))
          | otherwise -> do
              multiLineLoop (input:ls)

-- | Parse and run the command corresponding to some REPL input.
handleCMD :: Members DiscoEffects r => String -> Sem r ()
handleCMD "" = return ()
handleCMD s = do
  exts <- gets @TopInfo (view extSet)
  case parseLine discoCommands exts s of
    Left msg -> outputLn msg
    Right l  -> catch @DiscoError (dispatch discoCommands l) printoutLn
                -- The above has to be catch, not outputErrors, because
                -- the latter won't resume afterwards.
