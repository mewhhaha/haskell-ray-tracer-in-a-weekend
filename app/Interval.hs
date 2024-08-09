{-# LANGUAGE OverloadedRecordDot #-}

module Interval (Interval (Interval, min, max), contains, universe, size, inside) where

import Data.Ord qualified as Prelude
import GHC.Real (infinity)

data Interval = Interval
  { min :: Double,
    max :: Double
  }

size :: Interval -> Double
size interval = interval.max - interval.min

contains :: Interval -> Double -> Bool
contains interval value = value >= interval.min && value <= interval.max

inside :: Double -> Interval -> Double
inside value interval = Prelude.clamp (interval.min, interval.max) value

universe :: Interval
universe = Interval (-fromRational infinity) (fromRational infinity)

instance Semigroup Interval where
  (<>) i1 i2 = Interval (Prelude.min i1.min i2.min) (Prelude.max i1.max i2.max)

instance Monoid Interval where
  mempty = Interval 0 0