{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Data.Colour.Palette.BrewerSet
import           Data.List                     (intersperse)
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

type GCDState = ([Int], Int, Bool)

gcdStep ([a],0,p)   = ([a],0,p)
gcdStep ([x,y],b,p) = ([y],b,p)
gcdStep ([a],b,p)
  | a < b     = ([b],a, not p)
  | otherwise = ([b,a-b], b, p)

gcdViz :: GCDState -> Diagram B
gcdViz (as,b,p) =
  vsep 0.3
  [ case as of
      [a]    -> mkRect (fromIntegral a) c1
      [a, a'] ->
        hsep 0.25
        [ rect (fromIntegral a) 1 # alignL # lc c1 # lw medium # fc white
        , mkRect (fromIntegral a') c1
        ]
  , mkRect (fromIntegral b) c2
  ]
  # lw none
  where
    mkRect x c = rect x 1 # alignL # fc c
    [c1,c2] = (if p then reverse else id) . take 2 $ brewerSet Paired 3

main = defaultMain
  $ iterate gcdStep ([15],42, False)
  # take 9
  # map gcdViz
  # intersperse (arrowV' (with & headLength .~ local 1) (2 *^ unit_Y) # translateX 6)
  # vsep 1
  # frame 1
