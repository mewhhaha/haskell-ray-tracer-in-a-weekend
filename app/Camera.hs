module Camera (render, mkCamera, Camera (viewport, window), Window (width, height, ratio), Sampling (..), Viewport, mkWindow, pixels, Look (..), Size (..), Focus (..)) where

import Color
import Control.Monad.State
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.Functor ((<&>))
import Interval
import Ray (CanHit, Hit (Hit), Hittable (hit), Material (scatter), Ray (Ray), material, t)
import System.Random (Random (randomR, randomRs), RandomGen (split), StdGen)
import V3 (P3 (..), V3 (..))
import V3 qualified
import Window
import World (World, env, rng)

data DefocusDisk = DefocusDisk
  { u :: V3 Double,
    v :: V3 Double
  }

data Viewport = Viewport
  { width :: Double,
    height :: Double
    -- u :: V3 Double,
    -- v :: V3 Double
  }

mkViewport :: Double -> Window -> Viewport
mkViewport height window = Viewport {width, height}
  where
    width = height * (window.width / window.height)

data Samples = Samples
  { count :: Int,
    scale :: Double,
    bounces :: Int
  }

mkSamples :: Int -> Int -> Samples
mkSamples count = Samples count (1.0 / fromIntegral count)

data Camera = Camera
  { window :: Window,
    viewport :: Viewport,
    du :: V3 Double,
    dv :: V3 Double,
    top_left :: V3 Double,
    samples :: Samples,
    look_from :: P3 Double,
    look_at :: P3 Double,
    vfov :: Double,
    vup :: V3 Double,
    defocus_disk :: DefocusDisk,
    defocus_angle :: Double
  }

data Look = Look
  { look_from :: P3 Double,
    look_at :: P3 Double,
    up :: V3 Double,
    fov :: Double
  }

data Focus = Focus
  { focus_distance :: Double,
    defocus_angle :: Double
  }

data Sampling = Sampling
  { samples :: Int,
    bounces :: Int
  }

mkCamera :: Window -> Look -> Focus -> Sampling -> Camera
mkCamera window Look {look_from, look_at, up, fov} Focus {focus_distance, defocus_angle} Sampling {samples, bounces} = do
  -- top left of the viewport
  let theta = degreesToRadians fov
  let h = tan (theta / 2)

  let viewport = mkViewport (2.0 * h * focus_distance) window

  let w = V3.unit (look_from.v3 - look_at.v3)
  let u = V3.unit (V3.cross up w)
  let v = V3.cross w u

  let viewport_u = fmap (viewport.width *) u
  let viewport_v = fmap (viewport.height *) (-v)

  let pixel_delta_u = viewport_u <&> (/ window.width)
  let pixel_delta_v = viewport_v <&> (/ window.height)

  let viewport_top_left = look_from.v3 - fmap (focus_distance *) w - fmap (/ 2) (viewport_u + viewport_v)

  let defocus_radius = focus_distance * tan (degreesToRadians $ defocus_angle / 2)
  let defocus_disk =
        DefocusDisk
          { u = fmap (defocus_radius *) u,
            v = fmap (defocus_radius *) v
          }

  let pixels_top_left = viewport_top_left + fmap (0.5 *) (pixel_delta_u + pixel_delta_v)

  Camera
    { look_from,
      look_at,
      window,
      viewport,
      du = pixel_delta_u,
      dv = pixel_delta_v,
      -- top left of the viewport + offset to the first pixel
      top_left = pixels_top_left,
      samples = mkSamples samples bounces,
      vfov = fov,
      vup = up,
      defocus_angle = defocus_angle,
      defocus_disk
    }

newtype Texture
  = Texture
  { pixels :: [V3 Int]
  }

generators :: StdGen -> [StdGen]
generators g = g' : generators g''
  where
    (g', g'') = split g

render :: Camera -> World Texture
render camera = do
  objects <- gets env
  (g, g') <- gets (split . rng)
  modify $ \w -> w {rng = g'}

  let height = camera.window.height
  let width = camera.window.width

  let du = camera.du
  let dv = camera.dv

  let gs = generators g

  let uvs = do
        v <- fmap (\j -> fmap (j *) dv) [0 .. height - 1]
        u <- fmap (\i -> fmap (i *) du) [0 .. width - 1]

        return $ u + v

  let pixels = fmap (draw camera objects) (zip gs uvs) `using` parListChunk (floor camera.window.width) rdeepseq

  return $ Texture pixels

draw :: forall t. (Foldable t) => Camera -> t Ray.CanHit -> (StdGen, V3 Double) -> V3 Int
draw camera objects (gen, uv) = do
  let du = camera.du
  let dv = camera.dv
  let samples = camera.samples

  let (gen', gx, gy) = case take 3 $ generators gen of
        [a, b, c] -> (a, b, c)
        _ -> error "unreachable"

  let xs = take (samples.count - 1) (randomRs (-0.5, 0.5) gx)
  let ys = take (samples.count - 1) (randomRs (-0.5, 0.5) gy)

  let offsets = [fmap (x *) du + fmap (y *) dv | (x, y) <- zip xs ys]

  let rays =
        [ do
            let defocus_origin = fst $ defocusDiskSample gd camera
            let origin = if camera.defocus_angle <= 0 then camera.look_from.v3 else defocus_origin
            let pixel_sample = camera.top_left + uv + fmap (offset.x *) du + fmap (offset.y *) dv
            let direction = pixel_sample - origin

            Ray.Ray (P3 origin) direction
          | (gd, offset) <- zip (generators gen') (mempty : offsets)
        ]

  let sampled_square = [sample samples.bounces g ray objects | (g, ray) <- zip (generators gen) rays]

  let clear_color = rgb 0.5 0.7 1.0

  let color = mconcat (attenuate clear_color <$> sampled_square)

  bytes $ Color (fmap (samples.scale *) color.v3)

data PixelData = PixelData
  { attenuation :: V3 Double,
    ray :: Ray.Ray
  }

sample :: forall t. (Foldable t) => Int -> StdGen -> Ray.Ray -> t Ray.CanHit -> PixelData
sample depth g r world = do
  let iterations = take depth $ generators g

  case foldM bounce (r, V3.splat 1.0) iterations of
    Right (ray, _) -> PixelData {ray, attenuation = mempty}
    Left (ray, attenuation) -> PixelData {ray, attenuation}
  where
    bounce :: (Ray.Ray, V3 Double) -> StdGen -> Either (Ray.Ray, V3 Double) (Ray.Ray, V3 Double)
    bounce (ray, ray_color) g' = case cast ray universe {Interval.min = 0.001} world of
      Just h@Ray.Hit {material} -> do
        case Ray.scatter g' ray h material of
          Just (ray', attenuation) -> Right (ray', ray_color * attenuation.v3)
          Nothing -> Left (ray, mempty)
      Nothing -> Left (ray, ray_color)

attenuate :: Color -> PixelData -> Color
attenuate Color {v3} (PixelData {attenuation}) = Color $ attenuation * v3

cast :: forall t. (Foldable t) => Ray.Ray -> Interval -> t Ray.CanHit -> Maybe Ray.Hit
cast ray interval = foldl recast Nothing
  where
    recast Nothing c = Ray.hit ray interval c
    recast (Just h1) c = case Ray.hit ray interval c of
      Nothing -> Just h1
      Just h2 -> Just $ if h2.t < h1.t then h2 else h1

bytes :: Color -> V3 Int
bytes (Color c) = fmap (byteRange . gammaCorrected) c
  where
    byteRange :: Double -> Int
    byteRange v = floor . (* 256) $ v `inside` Interval 0 0.999

    gammaCorrected :: Double -> Double
    gammaCorrected v
      | v > 0 = sqrt v
      | otherwise = 0

randomInUnitDisk :: StdGen -> (V3 Double, StdGen)
randomInUnitDisk g = if sqrd > 0 then (V3.unit v3, g'') else randomInUnitDisk g''
  where
    sqrd = V3.lengthSquared v3
    v3 = V3 x y 0
    (x, g') = randomR (-1, 1) g
    (y, g'') = randomR (-1, 1) g'

defocusDiskSample :: StdGen -> Camera -> (V3 Double, StdGen)
defocusDiskSample g Camera {look_from, defocus_disk} = do
  let (p, g') = randomInUnitDisk g
  let s = look_from.v3 + fmap (p.x *) defocus_disk.u + fmap (p.y *) defocus_disk.v
  (s, g')

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * (pi / 180.0)
