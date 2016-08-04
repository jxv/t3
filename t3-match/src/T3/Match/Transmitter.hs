module T3.Match.Transmitter
  ( Transmitter(..)
  ) where

import T3.Core (Loc, XO)
import T3.Match.Types (Step)

class Monad m => Transmitter m where
  sendStep :: XO -> Step -> m ()
  recvLoc :: XO -> m Loc
