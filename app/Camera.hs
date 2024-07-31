module Camera (render, mkCamera, Camera (viewport, window), Window (width, height, ratio), Viewport, mkWindow, Ratio (Ratio), pixels) where

import Color
import Control.Monad.State
import Data.Functor ((<&>))
import Interval
import Ray (CanHit, Hit, Hittable (hit), Ray (Ray), direction, normal, t)
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
    scale :: Double
  }

mkSamples :: Int -> Samples
mkSamples count = Samples count (1.0 / fromIntegral count)

data Camera = Camera
  { center :: V3 Double,
    focalLength :: Double,
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
          focalLength = 1.0,
          window,
          viewport,
          du,
          dv,
          -- top left of the viewport + offset to the first pixel
          p00 = viewport00 + ((du + dv) <&> (* 0.5)),
          samples = mkSamples 10
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

render :: Camera -> World Texture
render camera = do
  objects <- gets env
  let du = camera.du
  let dv = camera.dv

  let uvs = do
        let height = camera.window.height
        let width = camera.window.width

        v <- [dv <&> (* j) | j <- [0 .. height - 1]]
        u <- [du <&> (* i) | i <- [0 .. width - 1]]
        return (u, v)

  pixels <- forM uvs $ \(u, v) -> do
    let center = camera.p00 + u + v

    (gx, gy) <- gets (split . rng)
    modify $ \w -> w {rng = snd (split gy)}

    let xs = take camera.samples.count (randomRs (-0.5, 0.5) gx)
    let ys = take camera.samples.count (randomRs (-0.5, 0.5) gy)

    let offsets = [(du <&> (* x)) + (dv <&> (* y)) | (x, y) <- zip xs ys]

    let baseRay = Ray (P3 camera.center) direction
          where
            direction = center - camera.center

    let randomRays =
          [ baseRay {direction = baseRay.direction + offset}
            | offset <- offsets
          ]

    let color = mconcat [sample ray objects | ray <- randomRays]
    return $ bytes $ Color (color.v3 <&> (* camera.samples.scale))

  return $ Texture pixels

sample :: forall t. (Foldable t) => Ray -> t CanHit -> Color
sample ray world = case cast ray universe {Interval.min = 0} world of
  Just h -> Color $ fmap (+ 1) h.normal <&> (* 0.5)
  Nothing -> Color blended
    where
      start = splat 1.0
      end = V3 0.5 0.7 1.0
      blended = (start <&> (* (1.0 - a))) + (end <&> (* a))
      unitDirection = V3.unit ray.direction
      a = 0.5 * (unitDirection.y + 1.0)

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
