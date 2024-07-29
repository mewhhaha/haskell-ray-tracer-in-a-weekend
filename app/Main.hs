{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Text.Printf (printf)
import V3 (V3 (V3, x, y, z), unit)

newtype Color = Color (V3 Double)

newtype ColorByte = ColorByte (V3 Int)

data Ratio = Ratio Int Int

newtype P3 d = P3 (V3 d)

data Ray = Ray
  { origin :: P3 Double,
    direction :: V3 Double
  }

data Window = Window
  { ratio :: Double,
    height :: Int,
    width :: Int
  }

data Viewport = Viewport
  { width :: Double,
    height :: Double,
    u :: V3 Double,
    v :: V3 Double
  }

data Camera = Camera
  { translation :: V3 Double,
    focalLength :: Double
  }

makeWindow :: Ratio -> Int -> Window
makeWindow (Ratio rw rh) width = Window ratio height width
  where
    ratio = fromIntegral rw / fromIntegral rh
    height = max 1 $ floor (fromIntegral width / ratio)

makeViewport :: Double -> Window -> Viewport
makeViewport height window = Viewport {width, height, u = V3 width 0 0, v = V3 0 (-height) 0}
  where
    width = height * (fromIntegral window.width / fromIntegral window.height)

at :: Ray -> Double -> P3 Double
at (Ray (P3 origin) direction) t = P3 (origin + fmap (* t) direction)

sample :: Ray -> Color
sample ray = Color $ start <&> (* (1.0 - a)) + (end <&> (* a))
  where
    start = V3 1.0 1.0 1.0
    end = V3 0.5 0.7 1.0
    unitDirection = V3.unit ray.direction
    a = 0.5 * (unitDirection.y + 1.0)

main :: IO ()
main = do
  let window = makeWindow (Ratio 16 9) 400

  let viewport = makeViewport 2.0 window

  let camera =
        Camera
          { translation = V3 0 0 0,
            focalLength = 1.0
          }

  let pixelDeltaU = viewport.u <&> (/ fromIntegral window.width)
  let pixelDeltaV = viewport.v <&> (/ fromIntegral window.height)

  let viewportUpperLeft = camera.translation - V3 0 0 camera.focalLength - (viewport.u <&> (/ 2)) + (viewport.v <&> (/ 2))

  let pixel00LOC = viewportUpperLeft + ((pixelDeltaU - pixelDeltaV) <&> (* 0.5))

  let pixels = do
        let w = window.width
        let h = window.height
        j <- [0 .. w - 1]
        i <- [0 .. h - 1]

        let pixelCenter = pixel00LOC + (pixelDeltaU <&> (* fromIntegral i)) + (pixelDeltaV <&> (* fromIntegral j))
        let rayDirection = pixelCenter - camera.translation
        let ray = Ray (P3 camera.translation) rayDirection

        return $ byteRange (sample ray)

  let header = formatHeader window
  let colors = concatMap formatColor pixels

  putStr $ header <> colors
  where
    formatColor (ColorByte (V3 r g b)) = printf "%d %d %d\n" r g b
    formatHeader window = printf "P3\n%d %d\n255\n" window.width window.height

byteRange :: Color -> ColorByte
byteRange (Color (V3 r g b)) = ColorByte (V3 (u8 r) (u8 g) (u8 b))
  where
    u8 :: Double -> Int
    u8 = floor . (255.999 *)

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap