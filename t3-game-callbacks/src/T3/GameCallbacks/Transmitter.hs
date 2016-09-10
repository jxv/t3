module T3.GameCallbacks.Transmitter
  ( Transmitter(..)
  ) where

import T3.Core (XO, Loc)
import T3.GameCallbacks.Types (Step)

class Monad m => Transmitter m where
  sendStep :: XO -> Step -> m ()
  recvLoc :: XO -> m Loc
