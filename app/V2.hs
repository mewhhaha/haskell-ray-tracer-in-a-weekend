module V2 (V2 (V2, x, y), V2.length, dot, unit, merge, splat) where

data V2 d = V2 {x :: d, y :: d}

instance Num (V2 Double) where
  (+) = merge (+)
  (*) v1 v2 = fmap (sum . merge (*) v2 . splat) v1
  abs = fmap abs
  signum = fmap signum
  fromInteger v = splat (fromInteger v)
  negate = fmap negate

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)

instance Foldable V2 where
  foldr f b (V2 x y) = f x (f y b)

length :: V2 Double -> Double
length = sqrt . sum . fmap (\x -> x * x)

dot :: V2 Double -> V2 Double -> Double
dot v1 v2 = sum $ merge (*) v1 v2

unit :: V2 Double -> V2 Double
unit v = fmap (/ V2.length v) v

merge :: (a -> b -> c) -> V2 a -> V2 b -> V2 c
merge f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)

splat :: a -> V2 a
splat x = V2 x x