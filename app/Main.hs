module Main (main) where

import Camera (Focus (..), Look (..), Sampling (..), Size (..), height, mkCamera, mkWindow, pixels, render, window)
import Color (rgb)
import Control.Monad.State (evalState)
import Material (Dialectric (..), Lambertian (..), Metal (..))
import Ray (CanHit (CanHit))
import Sphere (mkSphere)
import System.Random (mkStdGen)
import Text.Printf (printf)
import V3 (P3 (v3), V3 (V3), mkP3, up)
import Window qualified
import World (WorldState (WorldState))

scene1 = do
  let material_ground = Lambertian {albedo = rgb 0.8 0.8 0.0}
  let material_center = Lambertian {albedo = rgb 0.1 0.2 0.5}
  let material_left = Dialectric {refraction_index = 1.5}
  let material_bubble = Dialectric {refraction_index = 1 / 1.5}
  let material_right = Metal {albedo = rgb 0.8 0.6 0.2, fuzz = 1.0}

  [ CanHit $ mkSphere (0, -100.5, -1) 100 material_ground,
    CanHit $ mkSphere (0, 0, -1.2) 0.5 material_center,
    CanHit $ mkSphere (-1, 0, -1) 0.5 material_left,
    CanHit $ mkSphere (-1, 0, -1) 0.4 material_bubble,
    CanHit $ mkSphere (1, 0, -1) 0.5 material_right
    ]

scene2 = do
  let r = cos (pi / 4)
  let material_left = Lambertian {albedo = rgb 0 0 1}
  let material_right = Lambertian {albedo = rgb 1 0 0}

  [ CanHit $ mkSphere (-r, 0, -1) r material_left,
    CanHit $ mkSphere (r, 0, -1) r material_right
    ]

main :: IO ()
main = do
  let window = mkWindow Window.Size {width = 400, ratio = (16, 9)}
  let camera =
        mkCamera
          window
          Camera.Look
            { look_from = mkP3 (-2) 2 1,
              look_at = mkP3 0 0 (-1),
              up = V3.up,
              fov = 20
            }
          Camera.Focus
            { defocus_angle = 10.0,
              focus_distance = 3.4
            }
          Camera.Sampling
            { samples = 100,
              bounces = 50
            }

  let width = camera.window.width
  let height = camera.window.height

  let rng = mkStdGen 0
  let world = WorldState scene1 rng

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
