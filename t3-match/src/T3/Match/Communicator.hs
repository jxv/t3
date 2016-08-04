module T3.Match.Communicator
  ( Communicator(..)
  ) where

import T3.Core (XO, Loc, Board, Result)
import T3.Match.Types (Final)

class Monad m => Communicator m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Result -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()
