{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Diagrams.Prelude             (Diagram)
import qualified Diagrams.Backend.CmdLine     as D
import qualified Diagrams.Backend.SVG.CmdLine as SVG
import           Data.Text (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified Options.Applicative          as O

import Diagrams.Spirograph (Spiro(Spiro))
import qualified Diagrams.Spirograph as Spiro

getSpiros ∷ FilePath → IO [Spiro]
getSpiros p = fmap (step . Text.words) . Text.lines <$> Text.readFile p
  where
  step ∷ [Text] → Spiro
  step = \case
    (x:y:_) → Spiro (tRead x) (tRead y)
    _       → error "Parse error"

  -- What's the efficient implementation of this?
  tRead ∷ Read a ⇒ Text → a
  tRead = read . Text.unpack

data Options = Options
  { traceIntermediate ∷ Bool
  , inputFile         ∷ FilePath
  }

instance D.Parseable Options where
  parser
    = Options
    <$> O.switch
      (  O.long "trace-intermediate"
      <> O.help "Show intermediate disks"
      )
    <*> O.strOption
      (  O.long    "input"
      <> O.short   'i'
      <> O.help    "input file"
      <> O.metavar "FILE"
      )

draw ∷ Options → IO (Diagram SVG.B)
draw Options{..} = do
  xs ← getSpiros inputFile
  pure $ if
    | traceIntermediate → Spiro.drawSpirosIntermediate xs
    | otherwise         → Spiro.drawSpiros xs

main ∷ IO ()
main = SVG.mainWith draw
