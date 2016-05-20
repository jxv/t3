module T3.Match.Class
  ( Match(..)
  ) where

import T3.Game (XO, Loc, Board, Result)
import T3.Match.Types (Final)

class Match m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Result -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()
