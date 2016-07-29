module T3.Server.MatchTransmitterImpl
  ( sendStep
  , recvLoc
  ) where

import T3.Core (Loc)
import T3.Server (Step)
import T3.Server.Connection (Connection)
import T3.Server.ConnectionCallback (ConnectionCallback(..))

sendStep :: ConnectionCallback m => Connection -> Step -> m ()
sendStep connection step = do
  respond <- getRespond connection
  respond step

recvLoc :: ConnectionCallback m => Connection -> m Loc
recvLoc connection = do
  (loc, respond) <- getRequest connection
  putRespond connection respond
  return loc
