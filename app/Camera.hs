module Camera (render, mkCamera, Camera, Window (width, height, ratio), mkWindow, Ratio (Ratio), pixels) where

import Color
import Data.Functor ((<&>))
import Interval
import Ray (CanHit, Hit, Hittable (hit), Ray (Ray), direction, normal, t)
import V3

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

viewport00 :: Camera -> Viewport -> V3 Double
viewport00 camera viewport = camera.center - V3 0 0 camera.focalLength - ((viewport.u + viewport.v) <&> (/ 2))

data Camera = Camera
  { center :: V3 Double,
    focalLength :: Double
  }

mkCamera :: V3 Double -> Camera
mkCamera center = Camera center 1.0

newtype Texture
  = Texture
  { pixels :: [V3 Int]
  }

render :: forall t. (Foldable t) => Camera -> Window -> t CanHit -> Texture
render camera window world = Texture $ do
  let viewport = mkViewport 2.0 window

  let du = viewport.u <&> (/ window.width)
  let dv = viewport.v <&> (/ window.height)

  let p00 = viewport00 camera viewport + ((du + dv) <&> (* 0.5))

  v <- [dv <&> (* j) | j <- [0 .. window.height - 1]]
  u <- [du <&> (* i) | i <- [0 .. window.width - 1]]

  let center = p00 + u + v
  let ray = Ray (P3 camera.center) direction
        where
          direction = center - camera.center

  let color = case cast ray universe {Interval.min = 0} world of
        Nothing -> sampleBackground ray
        Just h -> sampleHit h
  return $ bytes color

cast :: forall t. (Foldable t) => Ray -> Interval -> t CanHit -> Maybe Hit
cast ray interval = foldl recast Nothing
  where
    recast Nothing c = hit ray interval c
    recast (Just h1) c = case hit ray interval {Interval.max = h1.t} c of
      Nothing -> Just h1
      Just h2 -> Just $ if h2.t < h1.t then h2 else h1

sampleHit :: Hit -> Color
sampleHit h = Color $ fmap (+ 1) h.normal <&> (* 0.5)

sampleBackground :: Ray -> Color
sampleBackground ray = Color blended
  where
    start = V3 1.0 1.0 1.0
    end = V3 0.5 0.7 1.0
    blended = (start <&> (* (1.0 - a))) + (end <&> (* a))
    unitDirection = V3.unit ray.direction
    a = 0.5 * (unitDirection.y + 1.0)

bytes :: Color -> V3 Int
bytes (Color (V3 r g b)) = V3 (u8 r) (u8 g) (u8 b)
  where
    u8 :: Double -> Int
    u8 = floor . (255.999 *)
