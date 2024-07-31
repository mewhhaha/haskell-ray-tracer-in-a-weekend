{-# LANGUAGE OverloadedRecordDot #-}

module Interval (Interval (Interval, min, max), contains, universe, size) where

import GHC.Real (infinity)

data Interval = Interval
  { min :: Double,
    max :: Double
  }

size :: Interval -> Double
size interval = interval.max - interval.min

contains :: Interval -> Double -> Bool
contains interval value = value >= interval.min && value <= interval.max

universe :: Interval
universe = Interval (-fromRational infinity) (fromRational infinity)

instance Semigroup Interval where
  (<>) i1 i2 = Interval (Prelude.min i1.min i2.min) (Prelude.max i1.max i2.max)

instance Monoid Interval where
  mempty = Interval 0 0