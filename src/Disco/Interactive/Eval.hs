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

import           Control.Lens               (view)
import           Control.Monad.Except

import           Disco.AST.Generic          (selectSide)
import           Disco.Error
import           Disco.Eval
import           Disco.Interactive.Commands
import           Disco.Interactive.Parser   (parseLine)
import           Disco.Value

import           Disco.Effects.Output
import           Disco.Effects.Store
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.State

showVal :: Int -> Value -> String
showVal 0 _           = "_"
showVal _ (VNum _ r)  = show r
showVal _ VUnit       = "()"
showVal k (VInj s v)  = selectSide s "L" "R" ++ showVal (k-1) v
showVal k (VPair u v) = "(" ++ showVal k u ++ ", " ++ showVal k v ++ ")"
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

printMem :: Members '[Input TopInfo, Store Cell, Output String] r => Sem r ()
printMem = do
  env <- inputs (view topEnv)

  printout env

  as <- assocsStore
  forM_ as $ \(k, Cell v _) ->
    outputLn $ show k ++ ": " ++ showVal 3 v

handleCMD :: Members DiscoEffects r => String -> Sem r ()
handleCMD "" = return ()
handleCMD s = do
    exts <- gets @TopInfo (view extSet)
    case parseLine discoCommands exts s of
      Left msg -> outputLn msg
      Right l  -> catch @DiscoError (dispatch discoCommands l) printoutLn
                  -- The above has to be catch, not outputErrors, because
                  -- the latter won't resume afterwards.
