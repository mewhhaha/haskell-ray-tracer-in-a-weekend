module World (World, WorldState (WorldState, env, rng)) where

import Control.Monad.State (State)
import Data.Vector (Vector)
import Ray (CanHit)
import System.Random (StdGen)

data WorldState = WorldState
  { env :: (Vector CanHit),
    rng :: StdGen
  }

type World = State WorldState