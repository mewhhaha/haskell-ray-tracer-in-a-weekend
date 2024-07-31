module Color (Color (Color, v3), rgb) where

import V3

newtype Color = Color {v3 :: V3 Double}

instance Semigroup Color where
  (<>) (Color v1) (Color v2) = Color $ v1 + v2

instance Monoid Color where
  mempty = Color $ V3 0 0 0

rgb :: Double -> Double -> Double -> Color
rgb r g b = Color $ V3 r g b
