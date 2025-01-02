-- |
-- Module:     DiscoWASM
-- Copyright:  (c) Brent Yorgey 2025, Sergey Vinokurov 2024
-- License:    BSD-3, Apache-2.0 (see LICENSE, pointfree-wasm-LICENSE)

module DiscoWASM (discoWasm) where

import Control.Monad
import Data.Foldable
import GHC.Wasm.Prim

import Disco.Interactive.CmdLine

discoBasic :: String -> String
discoBasic = ('3':)

-- Ignore output here
foreign import javascript unsafe "new Array"
  js_emptyArr :: IO JSVal

foreign import javascript unsafe "$1.push($2)"
  js_push :: JSVal -> JSString -> IO JSVal

foreign export javascript "pointfreeWasm"
  pointfreeWasm :: JSString -> IO JSVal

mkArr :: IO JSVal
mkArr = js_emptyArr

pushArr :: JSVal -> JSString -> IO ()
pushArr arr str = void $ js_push arr str

pointfreeWasm :: JSString -> IO JSVal
pointfreeWasm input = do
  let input' = fromJSString input
  let result :: String
      result = discoBasic input'
  pure $ toJSString result
