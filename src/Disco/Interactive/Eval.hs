{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for things entered at an interactive disco prompt.
--
-----------------------------------------------------------------------------

module Disco.Interactive.Eval where

import           Capability.Error
import           Capability.State
import           Control.Lens               (view)
import           Control.Monad.Except
import qualified Data.IntMap                as IM
import           Data.List                  (intercalate)

import           Disco.Capability
import           Disco.Eval
import           Disco.Interactive.Commands
import           Disco.Interactive.Parser   (parseLine)
import           Disco.Util
import           Disco.Value

showVal :: Int -> Value -> String
showVal 0 _            = "_"
showVal _ (VNum _ r)   = show r
showVal k (VCons i vs) = "K" ++ show i ++ " [" ++ intercalate ", " (map (showVal (k-1)) vs) ++ "]"
showVal _ (VConst op) = show op
showVal _ VClos{}     = "<closure>"
showVal _ VPAp{}      = "<pap>"
showVal _ VThunk{}    = "<thunk>"
showVal _ (VIndir l)  = "-> " ++ show l
showVal _ VFun_{}     = "<fun>"
showVal _ VDelay_{}   = "<delay>"
showVal _ VBag{}      = "<bag>"
showVal _ VType{}     = "<type>"
showVal _ VProp{}     = "<prop>"
showVal _ VGraph{}    = "<graph>"
showVal _ VMap{}      = "<map>"

printMem :: Has '[St "top", St "mem", MonadIO] m => m ()
printMem = do
  env <- gets @"top" (view topEnv)
  mem <- get @"mem"

  io $ print env

  forM_ (IM.assocs mem) $ \(k, Cell v _) ->
    io $ putStrLn $ show k ++ ": " ++ showVal 3 v

handleCMD :: String -> Disco ()
handleCMD "" = return ()
handleCMD s = do
    exts <- get @"exts"
    case parseLine discoCommands exts s of
      Left msg -> io $ putStrLn msg
      Right l -> catch @"err" (dispatch discoCommands l) (io . print  {- XXX pretty-print error -})
