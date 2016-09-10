module T3.Game.Play
  ( Play(..)
  , play'
  ) where

import T3.Core (Result(Unfinished,Winner,Tie), XO(X,O), Board, valid, insertXO, result)
import T3.Game.BoardManager (BoardManager(isOpenLoc, insertAtLoc, getResult))
import T3.Game.Control (Control(move, forfeit, end, tie))
import T3.Game.Types (Win(Win), Lose(Lose))

class Monad m => Play m where
  play :: XO -> XO -> m ()

play' :: (Control m, BoardManager m, Play m) => XO -> XO -> m ()
play' p0 p1 = do
  loc <- move p0
  isValid <- isOpenLoc loc
  if isValid
    then do
      insertAtLoc loc p0
      res <- getResult
      case res of
        Unfinished -> play p1 p0
        Winner _ -> end (Win p0) (Lose p1)
        Tie -> tie
    else forfeit (Win p1) (Lose p0)
