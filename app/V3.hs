module V3 (V3 (V3, x, y, z), V3.length, dot, cross, unit) where

data V3 d = V3 {x :: d, y :: d, z :: d}

instance Num (V3 Double) where
  (+) (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
  (*) (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 * x2 + x1 * y2 + x1 * z2) (y1 * x2 + y1 * y2 + y1 * z2) (z1 * x2 + z1 * y2 + z1 * z2)
  abs (V3 x1 y1 z1) = V3 (abs x1) (abs y1) (abs z1)
  signum (V3 x1 y1 z1) = V3 (signum x1) (signum y1) (signum z1)
  fromInteger v = V3 (fromInteger v) (fromInteger v) (fromInteger v)
  negate (V3 x y z) = V3 (-x) (-y) (-z)

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Foldable V3 where
  foldr f b (V3 x y z) = f x (f y (f z b))

length :: V3 Double -> Double
length = sqrt . sum . fmap (\x -> x * x)

dot :: V3 Double -> V3 Double -> Double
dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: V3 Double -> V3 Double -> V3 Double
cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

unit :: V3 Double -> V3 Double
unit v = fmap (/ V3.length v) v
