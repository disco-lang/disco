{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

setExample = circle 1 # fc blue ||| circle 1 # fc red

functionExample = mempty

graphExample = mempty

examples = hsep 1 [setExample, functionExample, graphExample]

main = defaultMain $ examples # frame 0.2
