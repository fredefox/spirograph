{-# OPTIONS_GHC -Wall #-}
module Diagrams.Spirograph
  ( Spiro(Spiro)
  , drawSpiros
  , drawSpirosIntermediate
  ) where

import           Data.Complex                 (Complex(..), mkPolar)
import           Diagrams.Prelude             (Diagram, P2)
import qualified Diagrams.Backend.SVG.CmdLine as SVG
import qualified Diagrams.Prelude             as D
import           Data.Monoid                  (Sum(..))

data Spiro = Spiro
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
disk ∷ Spiro → Path P
disk Spiro{..} = Path $ \t → P $ Sum $ p2 $ mkPolar offset (speed * t)

disks ∷ [Spiro] → Path P
disks = foldMap disk

allSpiros ∷ [Spiro] → [Path P]
allSpiros = \case
  []     → mempty
  (x:xs) → px : map f pxs
    where
    px  = disk x
    pxs = allSpiros xs
    f   ∷ Path P → Path P
    f p = px <> p

-- | Form a point in diagrams in terms of a complex number.
p2 ∷ Complex a → P2 a
p2 (x :+ y) = D.p2 (x, y)

-- | Convert a path into a path.
sample ∷ Path P → [P2 Double]
sample (Path f) = runP . f <$> enumFromThenTo 0 0.1 100

drawSpiros ∷ [Spiro] → Diagram SVG.B
drawSpiros xs = D.fromVertices $ sample (disks xs)

drawSpirosIntermediate ∷ [Spiro] → Diagram SVG.B
drawSpirosIntermediate xs = foldMap step (allSpiros xs)
  where
  step p = D.fromVertices $ sample p
