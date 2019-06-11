{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Diagrams.Prelude             (Diagram)
import qualified Diagrams.Backend.CmdLine     as D
import qualified Diagrams.Backend.SVG.CmdLine as SVG
import           Data.Text (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Control.Applicative          (optional)
import qualified Options.Applicative          as O

import Diagrams.Spirograph (Spiro(Spiro))
import qualified Diagrams.Spirograph as Spiro

getSpiros ∷ Maybe FilePath → IO [Spiro]
getSpiros mp = fmap (step . Text.words) . Text.lines <$> getInput
  where
  getInput ∷ IO Text
  getInput = case mp of
    Nothing -> Text.getContents
    Just p -> Text.readFile p
  step ∷ [Text] → Spiro
  step = \case
    (x:y:_) → Spiro (tRead x) (tRead y)
    _       → error "Parse error"

  -- What's the efficient implementation of this?
  tRead ∷ Read a ⇒ Text → a
  tRead = read . Text.unpack

data Options = Options
  { traceIntermediate ∷ Bool
  , inputFile         ∷ Maybe FilePath
  }

instance D.Parseable Options where
  parser = Options <$> traceIntermediateParser <*> fileParser
    where
    traceIntermediateParser = O.switch
      (  O.long "trace-intermediate"
      <> O.help "Show intermediate disks"
      )
    fileParser = optional $ O.strOption
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
