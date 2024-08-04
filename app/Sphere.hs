module Sphere (mkSphere, Sphere) where

import Data.Foldable (find)
import Data.Functor ((<&>))
import Interval
import Ray
import V3

data Sphere = forall m. (Material m) => Sphere (P3 Double) Double m

mkSphere :: forall m. (Material m) => (Double, Double, Double) -> Double -> m -> Sphere
mkSphere (x, y, z) radius = Sphere (mkP3 x y z) (Prelude.max 0 radius)

instance Hittable Sphere where
  hit :: Ray -> Interval -> Sphere -> Maybe Hit
  hit ray interval (Sphere center radius material) = do
    let oc = center.v3 - ray.origin.v3

    let a = V3.lengthSquared ray.direction
        h = V3.dot ray.direction oc
        c = V3.lengthSquared oc - (radius * radius)

    discriminant <- let value = (h * h) - (a * c) in if value < 0 then Nothing else Just value

    let sqrtd = sqrt discriminant

    let root1 = (h - sqrtd) / a
        root2 = (h + sqrtd) / a

    t <- find (contains interval) [root1, root2]

    let p = at ray t

    let outward_normal = (p.v3 - center.v3) <&> (/ radius)

    let face = faceDirection outward_normal

    let normal = case face of
          Front -> outward_normal
          Back -> -outward_normal

    Just $ Hit {t, p, normal, face, material}
    where
      faceDirection outwardNormal =
        if V3.dot ray.direction outwardNormal < 0
          then Front
          else Back
