{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import GHC.Real (infinity)
import Text.Printf (printf)
import V2 (V2 (V2, x, y))
import V2 qualified
import V3 (V3 (V3, x, y, z))
import V3 qualified

newtype Color = Color {v3 :: V3 Double}

data Ratio = Ratio Int Int

newtype P3 d = P3
  { v3 :: V3 d
  }

mkP3 :: d -> d -> d -> P3 d
mkP3 x y z = P3 (V3 x y z)

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

mkWindow :: Ratio -> Int -> Window
mkWindow (Ratio rw rh) width = Window ratio (fromIntegral height) (fromIntegral width)
  where
    ratio = fromIntegral rw / fromIntegral rh
    height = max (1 :: Int) $ floor (fromIntegral width / ratio)

mkViewport :: Double -> Window -> Viewport
mkViewport height window = Viewport {width, height, u = V3 width 0 0, v = V3 0 (-height) 0}
  where
    width = height * (window.width / window.height)

at :: Ray -> Double -> P3 Double
at (Ray (P3 origin) direction) t = P3 (origin + fmap (* t) direction)

class Hittable a where
  hit :: Ray -> (Double, Double) -> a -> Maybe Hit

data Sphere = Sphere
  { center :: P3 Double,
    radius :: Double
  }

mkSphere :: P3 Double -> Double -> Sphere
mkSphere center radius = Sphere center (max 0 radius)

data Face = Front | Back

data Hit = Hit
  { t :: Double,
    p :: P3 Double,
    normal :: V3 Double,
    face :: Face
  }

instance Hittable Sphere where
  hit :: Ray -> (Double, Double) -> Sphere -> Maybe Hit
  hit ray (tmin, tmax) (Sphere center radius) = do
    let oc = center.v3 - ray.origin.v3

    let a = V3.lengthSquared ray.direction
        h = V3.dot ray.direction oc
        c = V3.lengthSquared oc - (radius * radius)

    discriminant <- let value = (h * h) - (a * c) in if value < 0 then Nothing else Just value

    let sqrtd = sqrt discriminant

    let root1 = (h - sqrtd) / a
        root2 = (h + sqrtd) / a

    let withinLimits r = if r > tmin && r < tmax then Just r else Nothing

    t <- findMaybe withinLimits [root1, root2]

    let p = at ray t

    let outwardNormal = (p.v3 - center.v3) <&> (/ radius)

    let face = faceDirection outwardNormal

    let normal = case face of
          Front -> outwardNormal
          Back -> -outwardNormal

    Just $ Hit {t, p, normal, face}
    where
      faceDirection outwardNormal =
        if V3.dot ray.direction outwardNormal < 0
          then Front
          else Back

findMaybe f = listToMaybe . mapMaybe f

sampleHit :: Hit -> Color
sampleHit Hit {normal} = Color $ fmap (+ 1) normal <&> (* 0.5)

sampleBackground :: Ray -> Color
sampleBackground ray = Color blended
  where
    start = V3 1.0 1.0 1.0
    end = V3 0.5 0.7 1.0
    blended = (start <&> (* (1.0 - a))) + (end <&> (* a))
    unitDirection = V3.unit ray.direction
    a = 0.5 * (unitDirection.y + 1.0)

data Collidable = forall a. (Hittable a) => Collidable a

instance Hittable Collidable where
  hit ray limits (Collidable a) = hit ray limits a

main :: IO ()
main = do
  let window = mkWindow (Ratio 16 9) 400

  let viewport = mkViewport 2.0 window

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

  let world =
        [ Collidable $ mkSphere (mkP3 0 0 (-1)) 0.5,
          Collidable $ mkSphere (mkP3 0 (-100.5) (-1)) 100
        ]

  let pixels = do
        j <- [0 .. window.height - 1]
        i <- [0 .. window.width - 1]

        let offsetU = pixelDelta.u <&> (* i)
        let offsetV = pixelDelta.v <&> (* j)
        let center = pixelOrigin + offsetU + offsetV
        let ray = Ray (P3 camera.center) direction
              where
                direction = center - camera.center

        let color = case cast ray (0, maxValue) world of
              Nothing -> sampleBackground ray
              Just h -> sampleHit h
        return $ rgb color

  putStr $ formatHeader (floor window.width) (floor window.height)
  forM_ pixels $ \pixel -> do
    putStr $ formatColor pixel
  where
    formatColor :: V3 Int -> String
    formatColor (V3 r g b) = printf "%d %d %d\n" r g b

    formatHeader :: Int -> Int -> String
    formatHeader = printf "P3\n%d %d\n255\n"

cast :: Ray -> (Double, Double) -> [Collidable] -> Maybe Hit
cast ray (tmin, tmax) = foldl recast Nothing
  where
    recast Nothing c = hit ray (tmin, tmax) c
    recast (Just h1) c = case hit ray (tmin, h1.t) c of
      Nothing -> Just h1
      Just h2 -> Just $ if h2.t < h1.t then h2 else h1

rgb :: Color -> V3 Int
rgb (Color (V3 r g b)) = V3 (u8 r) (u8 g) (u8 b)
  where
    u8 :: Double -> Int
    u8 = floor . (255.999 *)

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180

maxValue :: (RealFloat a) => a
maxValue = x
  where
    n = floatDigits x
    b = floatRadix x
    (_, u) = floatRange x
    x = encodeFloat (b ^ n - 1) (u - n)
