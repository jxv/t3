module T3.Match.MatchTransmitterImpl
  ( sendStep
  , recvLoc
  ) where

import T3.Core (Loc)
import T3.Match.Types (Step)
import T3.Match.Connection (Connection)
import T3.Match.ConnectionCallback (ConnectionCallback(..))

sendStep :: ConnectionCallback m => Connection -> Step -> m ()
sendStep connection step = do
  respond <- getRespond connection
  respond step

recvLoc :: ConnectionCallback m => Connection -> m Loc
recvLoc connection = do
  (loc, respond) <- getRequest connection
  putRespond connection respond
  return loc
