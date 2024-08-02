module Material (Lambertian (..), Metal (..), Dialectric (..)) where

import Color
import Ray (Face (..), Hit, Material (scatter), Ray (Ray), direction, face, normal, p)
import System.Random (StdGen)
import System.Random.Stateful (Random (randomR))
import V3

newtype Lambertian = Lambertian
  { albedo :: Color
  }

instance Material Lambertian where
  scatter :: StdGen -> Ray -> Hit -> Lambertian -> Maybe (Ray, Color)
  scatter g _ h Lambertian {albedo} = do
    let scatter_direction = h.normal + fst (randomUnit g)
    let scattered = Ray h.p (if isNearZero scatter_direction then h.normal else scatter_direction)
    let attenuation = albedo
    Just (scattered, attenuation)

data Metal = Metal
  { albedo :: Color,
    fuzz :: Double
  }

instance Material Metal where
  scatter :: StdGen -> Ray -> Hit -> Metal -> Maybe (Ray, Color)
  scatter g ray h Metal {albedo, fuzz} = do
    let reflected = reflect ray.direction h.normal
    let fuzzed = unit reflected + fmap (fuzz *) (fst (randomUnit g))
    let scattered = Ray (p h) fuzzed
    let attenuation = albedo

    if V3.dot fuzzed h.normal > 0
      then Just (scattered, attenuation)
      else Nothing

newtype Dialectric = Dialectric
  { refraction_index :: Double
  }

instance Material Dialectric where
  scatter :: StdGen -> Ray -> Hit -> Dialectric -> Maybe (Ray, Color)
  scatter g ray h Dialectric {refraction_index} = do
    let attenuation = rgb 1 1 1
    let ri = case h.face of
          Front -> 1 / refraction_index
          Back -> refraction_index

    let unit_direction = V3.unit ray.direction

    let cos_theta = min (V3.dot (-unit_direction) h.normal) 1
    let sin_theta = sqrt (1 - cos_theta * cos_theta)

    let random_reflection_value = fst (randomR (0, 1) g)
    let cannot_refract = ri * sin_theta > 1
    let must_reflect = reflectance cos_theta ri > random_reflection_value

    let reflected = reflect unit_direction h.normal
    let refracted = refract unit_direction h.normal ri

    let direction =
          if cannot_refract || must_reflect
            then reflected
            else refracted

    let scattered = Ray (p h) direction
    Just (scattered, attenuation)

randomOnHemisphere :: StdGen -> V3 Double -> (V3 Double, StdGen)
randomOnHemisphere g normal = if V3.dot vec normal > 0 then (vec, g') else (-vec, g')
  where
    (vec, g') = randomUnit g

randomUnit :: StdGen -> (V3 Double, StdGen)
randomUnit g = if sqrd > 0 then (unit v3, g''') else randomUnit g'''
  where
    sqrd = V3.lengthSquared v3
    v3 = V3 x y z
    (x, g') = randomR (-1, 1) g
    (y, g'') = randomR (-1, 1) g'
    (z, g''') = randomR (-1, 1) g''

reflect :: V3 Double -> V3 Double -> V3 Double
reflect v n = v - fmap ((V3.dot v n * 2) *) n

refract :: V3 Double -> V3 Double -> Double -> V3 Double
refract uv n etai_over_etat = do
  let cos_theta = min (V3.dot (-uv) n) 1
  let r_out_perp = fmap (etai_over_etat *) (uv + fmap (cos_theta *) n)
  let r_out_parallel = fmap ((-sqrt (abs (1 - lengthSquared r_out_perp))) *) n

  r_out_perp + r_out_parallel

reflectance :: Double -> Double -> Double
reflectance cosine refraction_index = do
  let r0 = (1 - refraction_index) / (1 + refraction_index)
  let r0' = r0 * r0
  r0' + (1 - r0') * ((1 - cosine) ** 5)