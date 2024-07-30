module V3 (V3 (V3, x, y, z), V3.length, lengthSquared, dot, unit, merge, splat) where

data V3 d = V3 {x :: d, y :: d, z :: d}

instance Num (V3 Double) where
  (+) = merge (+)
  (*) v1 v2 = fmap (sum . merge (*) v2 . splat) v1
  abs = fmap abs
  signum = fmap signum
  fromInteger v = splat (fromInteger v)
  negate = fmap negate

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Foldable V3 where
  foldr f b (V3 x y z) = f x (f y (f z b))

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
