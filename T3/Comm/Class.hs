module T3.Comm.Class
  ( Comm(..)
  ) where

import Prelude
import T3.Game.Types
import T3.Comm.Types
import T3.Game.Core

class Monad m => Comm m where
  sendGameState :: XO -> m ()
  recvMove :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Win XO -> Lose XO -> m ()
  updateBoard :: Board -> m ()
