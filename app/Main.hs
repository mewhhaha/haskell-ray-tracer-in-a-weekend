{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Printf (printf)
import V2 (V2 (V2, x, y))
import V2 qualified
import V3 (V3 (V3, x, y, z))
import V3 qualified

newtype Color = Color (V3 Double)

data Ratio = Ratio Int Int

newtype P3 d = P3 (V3 d)

data Ray = Ray
  { origin :: P3 Double,
    direction :: V3 Double
  }

data Window = Window
  { ratio :: Double,
    height :: Double,
    width :: Double
  }

data Viewport = Viewport
  { width :: Double,
    height :: Double,
    u :: V3 Double,
    v :: V3 Double
  }

data Camera = Camera
  { center :: V3 Double,
    focalLength :: Double
  }

data PixelDelta = PixelDelta
  { u :: V3 Double,
    v :: V3 Double
  }

makeWindow :: Ratio -> Int -> Window
makeWindow (Ratio rw rh) width = Window ratio (fromIntegral height) (fromIntegral width)
  where
    ratio = fromIntegral rw / fromIntegral rh
    height = max (1 :: Int) $ floor (fromIntegral width / ratio)

makeViewport :: Double -> Window -> Viewport
makeViewport height window = Viewport {width, height, u = V3 width 0 0, v = V3 0 (-height) 0}
  where
    width = height * (window.width / window.height)

at :: Ray -> Double -> P3 Double
at (Ray (P3 origin) direction) t = P3 (origin + fmap (* t) direction)

data BoundingVolume = Sphere
  { center :: P3 Double,
    radius :: Double
  }

hit :: Ray -> BoundingVolume -> Maybe Double
hit ray (Sphere center radius) =
  if discriminant < 0
    then Nothing
    else
      let t = (-b - sqrt discriminant) / (2.0 * a)
       in Just t
  where
    oc =
      let (P3 vc) = center
          (P3 vo) = ray.origin
       in vc - vo

    a = V3.dot ray.direction ray.direction
    b = -(2.0 * V3.dot oc ray.direction)
    c = V3.dot oc oc - radius * radius
    discriminant = b * b - 4 * a * c

sampleNormal :: Ray -> Double -> Color
sampleNormal ray t = Color (fmap (+ 1) normal <&> (* 0.5))
  where
    normal =
      let (P3 position) = at ray t
       in V3.unit (position - V3 0 0 (-1))

sample :: Ray -> Color
sample ray = Color blended
  where
    start = V3 1.0 1.0 1.0
    end = V3 0.5 0.7 1.0
    blended = (start <&> (* (1.0 - a))) + (end <&> (* a))
    unitDirection = V3.unit ray.direction
    a = 0.5 * (unitDirection.y + 1.0)

main :: IO ()
main = do
  let window = makeWindow (Ratio 16 9) 400

  let viewport = makeViewport 2.0 window

  let camera =
        Camera
          { center = V3 0 0 0,
            focalLength = 1.0
          }

  let pixelDelta =
        PixelDelta
          { u = viewport.u <&> (/ window.width),
            v = viewport.v <&> (/ window.height)
          }

  let viewportUpperLeft = camera.center - V3 0 0 camera.focalLength - (viewport.u <&> (/ 2)) - (viewport.v <&> (/ 2))

  -- At 0,0 of the upper left viewport
  let pixelOrigin = viewportUpperLeft + ((pixelDelta.u + pixelDelta.v) <&> (* 0.5))

  let sphere = Sphere {center = P3 (V3 0 0 (-1)), radius = 0.5}
  let world = [sphere]

  let pixels = do
        j <- [0 .. window.height - 1]
        i <- [0 .. window.width - 1]

        let offsetU = pixelDelta.u <&> (* i)
        let offsetV = pixelDelta.v <&> (* j)
        let center = pixelOrigin + offsetU + offsetV
        let ray = Ray (P3 camera.center) direction
              where
                direction = center - camera.center

        let color = case first (hit ray) world of
              Nothing -> sample ray
              Just t -> sampleNormal ray t
        return $ rgb color

  putStr $ formatHeader (floor window.width) (floor window.height)
  forM_ pixels $ \pixel -> do
    putStr $ formatColor pixel
  where
    formatColor :: V3 Int -> String
    formatColor (V3 r g b) = printf "%d %d %d\n" r g b

    formatHeader :: Int -> Int -> String
    formatHeader = printf "P3\n%d %d\n255\n"

rgb :: Color -> V3 Int
rgb (Color (V3 r g b)) = V3 (u8 r) (u8 g) (u8 b)
  where
    u8 :: Double -> Int
    u8 = floor . (255.999 *)

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

first :: (a1 -> Maybe a2) -> [a1] -> Maybe a2
first f vs = listToMaybe (mapMaybe f vs)