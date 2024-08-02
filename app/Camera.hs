{-# LANGUAGE AllowAmbiguousTypes #-}

module Camera (render, mkCamera, Camera (viewport, window), Window (width, height, ratio), Viewport, mkWindow, Ratio (Ratio), pixels) where

import Color
import Control.Monad.State
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.Functor ((<&>))
import Debug.Trace
import Interval
import Ray (CanHit, Hit (Hit), Hittable (hit), Material (scatter), Ray (Ray), direction, material, normal, p, t)
import System.Random (Random (randomR, randomRs), RandomGen (split), StdGen)
import V3
import World (World, env, rng)

data Ratio = Ratio Int Int

data Window = Window
  { ratio :: Double,
    height :: Double,
    width :: Double
  }

mkWindow :: (Int, Int) -> Int -> Window
mkWindow (rw, rh) width = Window ratio (fromIntegral height) (fromIntegral width)
  where
    ratio = fromIntegral rw / fromIntegral rh
    height = Prelude.max (1 :: Int) $ floor (fromIntegral width / ratio)

data Viewport = Viewport
  { width :: Double,
    height :: Double,
    u :: V3 Double,
    v :: V3 Double
  }

mkViewport :: Double -> Window -> Viewport
mkViewport height window = Viewport {width, height, u = V3 width 0 0, v = V3 0 (-height) 0}
  where
    width = height * (window.width / window.height)

data Samples = Samples
  { count :: Int,
    scale :: Double,
    bounces :: Int
  }

mkSamples :: Int -> Int -> Samples
mkSamples count = Samples count (1.0 / fromIntegral count)

data Camera = Camera
  { center :: V3 Double,
    focal_length :: Double,
    window :: Window,
    viewport :: Viewport,
    du :: V3 Double,
    dv :: V3 Double,
    p00 :: V3 Double,
    samples :: Samples
  }

mkCamera :: V3 Double -> (Int, Int) -> Int -> Camera
mkCamera center ratio width = camera
  where
    camera =
      Camera
        { center,
          focal_length = 1.0,
          window,
          viewport,
          du,
          dv,
          -- top left of the viewport + offset to the first pixel
          p00 = viewport00 + fmap (0.5 *) (du + dv),
          samples = mkSamples 100 50
        }
    -- top left of the viewport
    viewport00 = center - V3 0 0 focalLength - fmap (0.5 *) (viewport.u + viewport.v)
    focalLength = 1.0
    du = viewport.u <&> (/ window.width)
    dv = viewport.v <&> (/ window.height)
    window = mkWindow ratio width
    viewport = mkViewport 2.0 window

newtype Texture
  = Texture
  { pixels :: [V3 Int]
  }

generators :: StdGen -> [StdGen]
generators g = g : g' : generators g''
  where
    (g', g'') = split g

render :: Camera -> World Texture
render camera = do
  objects <- gets env
  (g, g') <- gets (split . rng)
  modify $ \w -> w {rng = g'}

  let du = camera.du
  let dv = camera.dv

  let height = camera.window.height
  let width = camera.window.width

  let gs = generators g

  let uvs = do
        v <- [fmap (j *) dv | j <- [0 .. height - 1]]
        u <- [fmap (i *) du | i <- [0 .. width - 1]]

        return (u + v)

  let pixels = fmap (draw camera objects) (zip gs uvs) `using` parListChunk 64 rdeepseq

  return $ Texture pixels

draw :: forall t. (Foldable t) => Camera -> t CanHit -> (StdGen, V3 Double) -> V3 Int
draw camera objects (gen, uv) = do
  let du = camera.du
  let dv = camera.dv
  let samples = camera.samples

  let (gx, gy) = split gen

  let center = camera.p00 + uv

  let xs = take (samples.count - 1) (randomRs (-0.5, 0.5) gx)
  let ys = take (samples.count - 1) (randomRs (-0.5, 0.5) gy)

  let offsets = [fmap (x *) du + fmap (y *) dv | (x, y) <- zip xs ys]

  let direction = center - camera.center

  let rays = [Ray (P3 camera.center) (direction + offset) | offset <- mempty : offsets]

  let sampled_square = [sample samples.bounces g ray objects | (g, ray) <- zip (generators gen) rays]
  let color = mconcat (shader <$> sampled_square)

  bytes $ Color (fmap (samples.scale *) color.v3)

data PixelData = PixelData
  { attenuation :: V3 Double,
    ray :: Ray
  }

sample :: forall t. (Foldable t) => Int -> StdGen -> Ray -> t CanHit -> PixelData
sample depth g r world = do
  let iterations = take depth $ generators g

  case foldM bounce (r, splat 1.0) iterations of
    Right (ray, _) -> PixelData {ray, attenuation = mempty}
    Left (ray, attenuation) -> PixelData {ray, attenuation}
  where
    bounce :: (Ray, V3 Double) -> StdGen -> Either (Ray, V3 Double) (Ray, V3 Double)
    bounce (ray, ray_color) g' = case cast ray universe {Interval.min = 0.001} world of
      Just h@Hit {material} -> do
        case scatter g' ray h material of
          Just (ray', attenuation) -> Right (ray', ray_color * attenuation.v3)
          Nothing -> Left (ray, mempty)
      Nothing -> Left (ray, ray_color)

shader :: PixelData -> Color
shader (PixelData {attenuation}) = Color $ attenuation * base
  where
    base = V3 0.5 0.7 1.0

-- unit_direction = unit ray.direction
-- a = 0.5 * (unit_direction.y + 1.0)
-- color = fmap ((1.0 - a) *) (splat 1) + fmap (a *) base

cast :: forall t. (Foldable t) => Ray -> Interval -> t CanHit -> Maybe Hit
cast ray interval = foldl recast Nothing
  where
    recast Nothing c = hit ray interval c
    recast (Just h1) c = case hit ray interval c of
      Nothing -> Just h1
      Just h2 -> Just $ if h2.t < h1.t then h2 else h1

bytes :: Color -> V3 Int
bytes (Color c) = fmap (byteRange . gammaCorrected) c
  where
    byteRange :: Double -> Int
    byteRange v = floor . (* 256) $ v `inside` Interval 0 0.999

    gammaCorrected :: Double -> Double
    gammaCorrected v
      | v > 0 = sqrt v
      | otherwise = 0

randomOnHemisphere :: StdGen -> V3 Double -> (V3 Double, StdGen)
randomOnHemisphere g normal = if V3.dot vec normal > 0 then (vec, g') else (-vec, g')
  where
    (vec, g') = randomUnit g

randomUnit :: StdGen -> (V3 Double, StdGen)
randomUnit g = if sqrd > 0 then (unit v3, g''') else randomUnit g'''
  where
    sqrd = V3.lengthSquared v3
    v3 = V3 x y z
    (x, g') = randomR (-1, 1) g
    (y, g'') = randomR (-1, 1) g'
    (z, g''') = randomR (-1, 1) g''
