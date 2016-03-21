module T3.Game
  ( module T3.Game.Core
  , module T3.Game.Class
  , module T3.Game.Types
  , run
  ) where

import Prelude
import T3.Game.Core
import T3.Game.Class
import T3.Game.Types

run :: Game m => Board -> m ()
run b = play b X O 

play :: Game m => Board -> XO -> XO -> m ()
play b p0 p1 = do
  loc <- move p0
  if not (valid loc b)
    then forfeit (Win p1) (Lose p0)
    else do
      let b' = insertXO loc p0 b
      step b' p0 loc
      case result b' of
        Unfinished -> play b' p1 p0
        Winner _ -> end (Win p0) (Lose p1)
        Tie -> tie
