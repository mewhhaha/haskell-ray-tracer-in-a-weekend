module Camera (render, mkCamera, Camera (viewport, window), Window (width, height, ratio), Viewport, mkWindow, Ratio (Ratio), pixels) where

import Color
import Control.Monad.State
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.Functor ((<&>))
import Debug.Trace
import Interval
import Ray (CanHit, Hit, Hittable (hit), Ray (Ray), normal, p, t)
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
          p00 = viewport00 + ((du + dv) <&> (* 0.5)),
          samples = mkSamples 100 10
        }
    -- top left of the viewport
    viewport00 = center - V3 0 0 focalLength - ((viewport.u + viewport.v) <&> (/ 2))
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
        v <- [dv <&> (* j) | j <- [0 .. height - 1]]
        u <- [du <&> (* i) | i <- [0 .. width - 1]]

        return (u, v)

  let pixels = fmap (draw camera objects) (zip gs uvs) `using` parListChunk 64 rdeepseq

  return $ Texture pixels

draw :: forall t. (Foldable t) => Camera -> t CanHit -> (StdGen, (V3 Double, V3 Double)) -> V3 Int
draw camera objects (gen, (u, v)) = do
  let du = camera.du
  let dv = camera.dv
  let samples = camera.samples

  let (gx, gy) = split gen

  let center = camera.p00 + u + v

  let xs = take (samples.count - 1) (randomRs (-0.5, 0.5) gx)
  let ys = take (samples.count - 1) (randomRs (-0.5, 0.5) gy)

  let offsets = [(du <&> (* x)) + (dv <&> (* y)) | (x, y) <- zip xs ys]

  let direction = center - camera.center

  let rays = [Ray (P3 camera.center) (direction + offset) | offset <- offsets]

  let pixel_datas = [sample samples.bounces g ray objects | (g, ray) <- zip (generators gen) rays]
  let additive = mconcat (shader <$> pixel_datas)

  let color = Color $ additive.v3 <&> (* samples.scale)

  bytes color

newtype PixelData = PixelData
  { color :: V3 Double
  }

sample :: forall t. (Foldable t) => Int -> StdGen -> Ray -> t CanHit -> PixelData
sample depth g r world = do
  let iterations = take depth $ generators g

  case foldM bounce (r, PixelData {color = splat 1.0}) iterations of
    Right (_, pixel_data) -> pixel_data
    Left pixel_data -> pixel_data
  where
    bounce (ray, pixel_data) g' = case cast ray universe {Interval.min = 0} world of
      Just h -> do
        let direction = fst $ randomOnHemisphere g' h.normal
        let ray' = Ray h.p direction
        let pixel_data' = PixelData {color = pixel_data.color <&> (* 0.5)}
        Right (ray', pixel_data')
      Nothing -> Left pixel_data

shader :: PixelData -> Color
shader (PixelData {color}) = Color $ V3.merge (*) base color
  where
    base = V3 0.5 0.7 1.0

cast :: forall t. (Foldable t) => Ray -> Interval -> t CanHit -> Maybe Hit
cast ray interval = foldl recast Nothing
  where
    recast Nothing c = hit ray interval c
    recast (Just h1) c = case hit ray interval {Interval.max = h1.t} c of
      Nothing -> Just h1
      Just h2 -> Just $ if h2.t < h1.t then h2 else h1

bytes :: Color -> V3 Int
bytes (Color c) = fmap u8 c
  where
    u8 :: Double -> Int
    u8 v = floor . (* 256) $ v `inside` Interval 0 0.999

randomOnHemisphere :: StdGen -> V3 Double -> (V3 Double, StdGen)
randomOnHemisphere g normal = if V3.dot vec normal > 0 then (vec, g') else (-vec, g')
  where
    (vec, g') = randomUnit g

randomUnit :: StdGen -> (V3 Double, StdGen)
randomUnit g = if sqrd < 1 && sqrd > 0 then (unit v3, g''') else randomUnit g'''
  where
    sqrd = V3.lengthSquared v3
    v3 = V3 x y z
    (x, g') = randomR (-1, 1) g
    (y, g'') = randomR (-1, 1) g'
    (z, g''') = randomR (-1, 1) g''
