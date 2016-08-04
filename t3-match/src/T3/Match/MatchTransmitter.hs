module T3.Match.MatchTransmitter
  ( MatchTransmitter(..)
  ) where

import T3.Core (Loc, XO)
import T3.Match.Types (Step)

class Monad m => MatchTransmitter m where
  sendStep :: XO -> Step -> m ()
  recvLoc :: XO -> m Loc
