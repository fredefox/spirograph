{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Data.Complex                 (Complex(..), mkPolar)
import           Diagrams.Prelude             (Diagram, P2)
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG.CmdLine as SVG
import           Data.Monoid                  (Sum(..))
import           Data.Text (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text

data Disk = Disk
  { offset ∷ Double
  , speed  ∷ Double
  }

newtype Path w = Path (Double → w)

deriving newtype instance Semigroup w ⇒ Semigroup (Path w)
deriving newtype instance Monoid w    ⇒ Monoid    (Path w)

newtype P = P (Sum (P2 Double))

runP ∷ P → P2 Double
runP (P (Sum p)) = p

deriving newtype instance Semigroup P
deriving newtype instance Monoid P

-- | @'disk' d@ is the path that @d@ will travel.
disk ∷ Disk → Path P
disk Disk{..} = Path $ \t → P $ Sum $ p2 $ mkPolar offset (speed * t)

disks ∷ [Disk] → Path P
disks = foldMap disk

-- | Form a point in diagrams in terms of a complex number.
p2 ∷ Complex a → P2 a
p2 (x :+ y) = D.p2 (x, y)

-- | Convert a path into a path.
sample ∷ Path P → [P2 Double]
sample (Path f) = runP . f <$> enumFromThenTo 0 0.1 100

drawDisks ∷ [Disk] → Diagram SVG.B
drawDisks xs = D.fromVertices $ sample (disks xs)

getDisks ∷ IO [Disk]
getDisks = fmap (step . Text.words) . Text.lines <$> Text.getContents
  where
  step ∷ [Text] → Disk
  step = \case
    (x:y:_) → Disk (tRead x) (tRead y)
    _       → error "Parse error"

-- What's the efficient implementation of this?
tRead ∷ Read a ⇒ Text → a
tRead = read . Text.unpack

main ∷ IO ()
main = do
  xs ← getDisks
  SVG.mainWith $ drawDisks xs
