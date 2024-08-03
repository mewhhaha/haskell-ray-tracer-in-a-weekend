{-# LANGUAGE DeriveGeneric #-}

module V3 (P3 (P3, v3), mkP3, V3 (V3, x, y, z), V3.length, lengthSquared, dot, unit, splat, isNearZero, cross, up) where

import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)

data V3 d = V3 {x :: d, y :: d, z :: d}
  deriving (Show, Generic)

instance (NFData d) => NFData (V3 d)

instance Num (V3 Double) where
  (+) = (<>)
  (*) = merge (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger v = splat (fromInteger v)
  negate = fmap negate

instance Functor V3 where
  fmap :: (a -> b) -> V3 a -> V3 b
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Foldable V3 where
  foldr f b (V3 x y z) = f x (f y (f z b))

instance Semigroup (V3 Double) where
  (<>) = merge (+)

instance Monoid (V3 Double) where
  mempty = splat 0

length :: V3 Double -> Double
length = sqrt . lengthSquared

lengthSquared :: V3 Double -> Double
lengthSquared = sum . fmap (\x -> x * x)

dot :: V3 Double -> V3 Double -> Double
dot v1 v2 = sum $ merge (*) v1 v2

-- cross :: V3 Double -> V3 Double -> V3 Double
-- cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

unit :: V3 Double -> V3 Double
unit v = fmap (/ V3.length v) v

merge :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
merge f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)

splat :: d -> V3 d
splat v = V3 v v v

cross :: V3 Double -> V3 Double -> V3 Double
cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

isNearZero :: V3 Double -> Bool
isNearZero v = let s = 1e-8 in all ((< s) . abs) v

newtype P3 d = P3
  { v3 :: V3 d
  }

mkP3 :: d -> d -> d -> P3 d
mkP3 x y z = P3 (V3 x y z)

up :: V3 Double
up = V3 0 1 0