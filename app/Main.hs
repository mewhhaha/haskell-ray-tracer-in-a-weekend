module Main (main) where

import Camera (Focus (..), Look (..), Sampling (..), Size (..), height, mkCamera, mkWindow, pixels, render, window)
import Color (Color (Color), rgb)
import Control.Monad.State (State, evalState, get, put)
import Data.Maybe (catMaybes)
import Material (Dialectric (..), Lambertian (..), Metal (..))
import Ray (CanHit (CanHit), Material)
import Sphere (mkSphere)
import System.Random (RandomGen (split), mkStdGen, randomRs)
import System.Random.Stateful (StdGen)
import Text.Printf (printf)
import V3 (V3 (..), length, mkP3, unit, up)
import Window qualified
import World (WorldState (WorldState))

type Doubles a = State [Double] a

pack :: forall a. (Material a) => V3 Double -> a -> CanHit
pack V3 {x, y, z} = CanHit . mkSphere (x, y, z) 0.2

getDouble :: Doubles Double
getDouble = do
  rs <- get
  let (value, next) = case rs of
        [] -> error "Ran out of random numbers"
        x : xs -> (x, xs)
  put next
  return value

randomColor :: Doubles Color
randomColor = do
  (r, g, b) <- do
    v <- getDouble
    v' <- getDouble
    v'' <- getDouble
    return (v, v', v'')
  return (Color (V3.unit (V3 r g b)))

randomDiffuse :: V3 Double -> Doubles CanHit
randomDiffuse at = do
  color <- randomColor
  let material = Lambertian color

  return $ pack at material

randomMetal :: V3 Double -> Doubles CanHit
randomMetal at = do
  color <- randomColor
  fuzz <- getDouble
  let material = Metal color fuzz
  return $ pack at material

scene :: StdGen -> [CanHit]
scene gen = do
  let ground_material = Lambertian $ rgb 0.5 0.5 0.5

  let ground = CanHit $ mkSphere (0, -1000, 0) 1000 ground_material

  let values = [(a, b) | a <- [-11 .. 11], b <- [-11 .. 11]]

  let random_sphere (a, b) = do
        center <- do
          v1 <- getDouble
          v2 <- getDouble
          return $ V3 (a + 0.9 * v1) 0.2 (b + 0.9 * v2)

        if V3.length (center - V3 4 0.2 0) < 0.9
          then return Nothing
          else do
            choose_mat <- getDouble
            sphere <- case choose_mat of
              v
                | v < 0.8 -> randomDiffuse center
                | v < 0.95 -> randomMetal center
                | otherwise -> return $ pack center (Dialectric 1.5) -- Glass
            return $ Just sphere

  let stream_of_values :: [Double] = randomRs (0.0, 1.0) gen
  let spheres = catMaybes $ evalState (mapM random_sphere values) stream_of_values

  let material_1 = Dialectric 1.5
  let big_glass_sphere = CanHit $ mkSphere (0, 1, 0) 1 material_1

  let material_2 = Lambertian $ rgb 0.4 0.2 0.1
  let big_diffuse_sphere = CanHit $ mkSphere (-4, 1, 0) 1 material_2

  let material_3 = Metal (rgb 0.7 0.6 0.5) 0.0
  let big_metal_sphere = CanHit $ mkSphere (4, 1, 0) 1 material_3

  ground : big_glass_sphere : big_diffuse_sphere : big_metal_sphere : spheres

main :: IO ()
main = do
  let window = mkWindow Window.Size {width = 1200, ratio = (16, 9)}
  let camera =
        mkCamera
          window
          Camera.Look
            { look_from = mkP3 13 2 3,
              look_at = mkP3 0 0 0,
              up = V3.up,
              fov = 20
            }
          Camera.Focus
            { defocus_angle = 0.6,
              focus_distance = 10.0
            }
          Camera.Sampling
            { samples = 500,
              bounces = 50
            }

  let width = camera.window.width
  let height = camera.window.height

  let (rng, rng') = split $ mkStdGen 0
  let world = WorldState (scene rng) rng'

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
