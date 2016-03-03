module T3.Game.Class where

import Prelude
import T3.Game.Types
import T3.Game.Core

class Monad m => Game m where
  move :: XO -> m Loc
  forfeit :: Win XO -> Lose XO -> m ()
  end :: Win XO -> Lose XO -> m ()
  tie :: m ()
  step :: Board -> m ()
