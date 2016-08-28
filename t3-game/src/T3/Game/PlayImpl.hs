module T3.Game.PlayImpl
  ( play'
  ) where

import T3.Core (Result(Unfinished,Winner,Tie), XO(X,O), Board, valid, insertXO, result)

import T3.Game.Types (Win(Win), Lose(Lose))
import T3.Game.Parts
  ( Play(play)
  , BoardManager(isOpenLoc, insertAtLoc, getResult)
  , Control(move, forfeit, end, tie)
  )

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
