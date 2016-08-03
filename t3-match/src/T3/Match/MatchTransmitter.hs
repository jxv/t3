module T3.Match.MatchTransmitter
  ( MatchTransmitter(..)
  ) where

import T3.Core (Loc)
import T3.Match.Types (Step)
import T3.Match.Connection (Connection)

class Monad m => MatchTransmitter m where
  sendStep :: Connection -> Step -> m ()
  recvLoc :: Connection -> m Loc
