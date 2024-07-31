module Interval (Interval (Interval), start, end, contains, universe, size) where

import GHC.Real (infinity)

data Interval = Interval
  { start :: Double,
    end :: Double
  }

size :: Interval -> Double
size (Interval start end) = end - start

contains :: Interval -> Double -> Bool
contains (Interval start end) value = value >= start && value <= end

universe :: Interval
universe = Interval (-fromRational infinity) (fromRational infinity)

instance Semigroup Interval where
  (<>) (Interval start1 end1) (Interval start2 end2) = Interval (min start1 start2) (max end1 end2)

instance Monoid Interval where
  mempty = Interval 0 0
  mconcat intervals = Interval (minimum $ map start intervals) (maximum $ map end intervals)