{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Camera (height, mkCamera, mkWindow, pixels, render, width)
import Control.Monad (forM_)
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import GHC.Real (infinity)
import Interval
import Ray (CanHit (CanHit))
import Sphere (mkSphere)
import Text.Printf (printf)
import V3 (V3 (V3, x, y, z), mkP3)
import V3 qualified

main :: IO ()
main = do
  let window = mkWindow (16, 9) 400

  let camera = mkCamera (V3.splat 0)

  let world =
        [ CanHit $ mkSphere (mkP3 0 0 (-1)) 0.5,
          CanHit $ mkSphere (mkP3 0 (-100.5) (-1)) 100
        ]

  let texture = render camera window world

  putStr $ formatHeader (floor window.width) (floor window.height)
  forM_ texture.pixels $ \pixel -> do
    putStr $ formatColor pixel
  where
    formatColor :: V3 Int -> String
    formatColor (V3 r g b) = printf "%d %d %d\n" r g b

    formatHeader :: Int -> Int -> String
    formatHeader = printf "P3\n%d %d\n255\n"

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180
