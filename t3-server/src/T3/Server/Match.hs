module T3.Server.Match
  ( Match(..)
  ) where

import T3.Core (XO, Loc, Board, Result)
import T3.Server (Final)

class Monad m => Match m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Result -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()
