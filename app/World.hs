module World (World, WorldState (WorldState, env, rng)) where

import Control.Monad.State (State)
import Ray (CanHit)
import System.Random (StdGen)

data WorldState = WorldState
  { env :: [CanHit],
    rng :: StdGen
  }

type World = State WorldState