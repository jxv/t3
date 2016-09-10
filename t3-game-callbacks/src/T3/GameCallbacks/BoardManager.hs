module T3.GameCallbacks.BoardManager
  ( insertAtLoc'
  , isOpenLoc'
  , getResult'
  ) where

import qualified T3.Game.BoardManager as Game
import T3.Core (Loc, XO, Result)
import T3.Game.HasBoard (HasBoard(getBoard))
import T3.GameCallbacks.Communicator (Communicator(logAction, updateBoard))

insertAtLoc' :: (HasBoard m, Communicator m) => Loc -> XO -> m ()
insertAtLoc' loc xo = do
  Game.insertAtLoc' loc xo
  logAction xo loc
  board <- getBoard
  updateBoard board

isOpenLoc' :: HasBoard m => Loc -> m Bool
isOpenLoc' = Game.isOpenLoc'

getResult' :: HasBoard m => m Result
getResult' = Game.getResult'
