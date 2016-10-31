{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

setExample = mconcat
  [ text "$A$" # translateX (-0.5)
  , text "$B$" # translateX (1.5)
  , text "$A \\cap B$" # translateX 0.5
  , circle 1 # fc blue # opacity 0.5
  , circle 1 # fc red  # opacity 0.5 # translateX 1
  ]
  # fontSizeL 0.25

functionExample = ["A" .>> set 4, "B" .>> set 5]
    # map centerY
    # hsep 1
    # applyAll (map conn [(0,0), (1,2), (2,4), (3,0)])
  where
    set n = vsep 0.2 (zipWith named [0::Int ..] $ replicate n dot)
    dot = circle 0.1 # fc black
    conn (x,y) = connectOutside ("A" .> (x :: Int)) ("B" .> (y :: Int))

graphExample :: Diagram B
graphExample
  = replicate 6 dot
  # zipWith named [0::Int ..]
  # atPoints [0.5 ^& 0, 1 ^& 1, 1 ^& 2, 2 ^& 0, 2.3 ^& 1, 2 ^& 2]
  # applyAll (map conn [(0,1), (1,2), (1,5), (2,5), (5,4), (1,3)])
  where
    dot = circle 0.05 # fc black
    conn (x,y) = connect' (with & arrowHead .~ noHead) (x :: Int) (y :: Int)

examples
  = [setExample, functionExample, graphExample]
  # map centerXY
  # hsep 1

main = defaultMain $ examples # frame 0.2
