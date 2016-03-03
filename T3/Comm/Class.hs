module T3.Comm.Class
  ( Comm(..)
  ) where


import Prelude
import T3.Game.Types
import T3.Comm.Types
import T3.Game.Core

class Monad m => Comm m where
  sendGameState :: UserId -> m ()
  recvMove :: UserId -> m Loc
  sendFinal :: UserId -> Final -> m ()
  updateBoard :: Board -> m ()
  ackInvalidMove :: UserId -> m ()
