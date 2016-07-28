module T3.Server.MatchTransmitter
  ( MatchTransmitter(..)
  ) where

import T3.Core (Loc)
import T3.Server.Connection (Connection)
import T3.Server (Step)

class Monad m => MatchTransmitter m where
  sendStep :: Connection -> Step -> m ()
  recvLoc :: Connection -> m Loc
