module Ray (Ray (Ray, origin, direction), at, hit, Hit (Hit, t, p, normal, face), Face (Front, Back), CanHit (CanHit), Hittable) where

import Interval
import V3

data Face = Front | Back

data Hit = Hit
  { t :: Double,
    p :: P3 Double,
    normal :: V3 Double,
    face :: Face
  }

data Ray = Ray
  { origin :: P3 Double,
    direction :: V3 Double
  }

at :: Ray -> Double -> P3 Double
at (Ray (P3 origin) direction) t = P3 (origin + fmap (* t) direction)

class Hittable a where
  hit :: Ray -> Interval -> a -> Maybe Hit

data CanHit = forall a. (Hittable a) => CanHit a

instance Hittable CanHit where
  hit ray limits (CanHit a) = hit ray limits a