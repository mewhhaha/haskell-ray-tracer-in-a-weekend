{-# LANGUAGE DerivingStrategies #-}

module Color (Color (Color, v3), rgb) where

import V3

newtype Color = Color {v3 :: V3 Double}
  deriving newtype (Semigroup, Monoid, Num)

rgb :: Double -> Double -> Double -> Color
rgb r g b = Color $ V3 r g b
