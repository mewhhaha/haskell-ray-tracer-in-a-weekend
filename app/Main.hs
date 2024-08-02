module Main (main) where

import Camera (height, mkCamera, pixels, render, width, window)
import Color (rgb)
import Control.Monad.State (evalState)
import Material (Dialectric (..), Lambertian (..), Metal (..))
import Ray (CanHit (CanHit))
import Sphere (mkSphere)
import System.Random (mkStdGen)
import Text.Printf (printf)
import V3 (V3 (V3), mkP3)
import World (WorldState (WorldState))

main :: IO ()
main = do
  let camera = mkCamera mempty (16, 9) 400
  let width = camera.window.width
  let height = camera.window.height

  let material_ground = Lambertian {albedo = rgb 0.8 0.8 0.0}
  let material_center = Lambertian {albedo = rgb 0.1 0.2 0.5}
  let material_left = Dialectric {refraction_index = 1.5}
  let material_right = Metal {albedo = rgb 0.8 0.6 0.2, fuzz = 1.0}

  let env =
        [ CanHit $ mkSphere (0, -100.5, -1) 100 material_ground,
          CanHit $ mkSphere (0, 0, -1.2) 0.5 material_center,
          CanHit $ mkSphere (-1, 0, -1) 0.5 material_left,
          CanHit $ mkSphere (1, 0, -1) 0.5 material_right
        ]

  let rng = mkStdGen 0
  let world = WorldState env rng

  let texture = evalState (render camera) world

  let header = formatHeader (floor width) (floor height)
  let body = formatColor <$> texture.pixels

  let output = header <> mconcat body

  putStr output
  where
    formatColor :: V3 Int -> String
    formatColor (V3 r g b) = printf "%d %d %d\n" r g b

    formatHeader :: Int -> Int -> String
    formatHeader = printf "P3\n%d %d\n255\n"

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180
