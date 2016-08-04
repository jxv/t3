module T3.Match.MatchTransmitterImpl
  ( sendStep
  , recvLoc
  ) where

import T3.Core (Loc, XO)
import T3.Match.Types (Step)
import T3.Match.Connection (Connection)
import T3.Match.ConnectionCallback (ConnectionCallback(..))

sendStep :: ConnectionCallback m => XO -> Step -> m ()
sendStep xo step = do
  respond <- getRespond xo
  respond step

recvLoc :: ConnectionCallback m => XO -> m Loc
recvLoc xo = do
  (loc, respond) <- getRequest xo
  putRespond xo respond
  return loc
