import           Development.Shake
import           Development.Shake.FilePath

lhs2TeX, pdflatex, mklatex :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"

main :: IO ()
main = shake shakeOptions $ do

    want ["epxlaining-errors.pdf"]

    "*.tex" *> \output -> do
      let input = output -<.> "lhs"
      need [input]
      cmd lhs2TeX $ ["--poly", "-o", output, input]

    "*.pdf" *> \output -> do
      let input = output -<.> "tex"
      need [input]
      cmd pdflatex $ ["--enable-write18", input]
