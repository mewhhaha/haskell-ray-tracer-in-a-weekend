{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Camera (height, mkCamera, pixels, render, width, window)
import Control.Monad (forM_)
import Ray (CanHit (CanHit))
import Sphere (mkSphere)
import Text.Printf (printf)
import V3 (V3 (V3, x, y, z), mkP3)
import V3 qualified

main :: IO ()
main = do
  let camera = mkCamera (V3.splat 0) (16, 9) 400

  let world =
        [ CanHit $ mkSphere (mkP3 0 0 (-1)) 0.5,
          CanHit $ mkSphere (mkP3 0 (-100.5) (-1)) 100
        ]

  let texture = render camera world

  putStr $ formatHeader (floor camera.window.width) (floor camera.window.height)
  forM_ texture.pixels $ \pixel -> do
    putStr $ formatColor pixel
  where
    formatColor :: V3 Int -> String
    formatColor (V3 r g b) = printf "%d %d %d\n" r g b

    formatHeader :: Int -> Int -> String
    formatHeader = printf "P3\n%d %d\n255\n"

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180
