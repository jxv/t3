module T3.GameCallbacks.BoardManager
  ( insertAtLoc
  ) where

import qualified T3.Game.BoardManager as BoardManager (insertAtLoc)
import T3.Core (Loc, XO)
import T3.Game.Classes (HasBoard(getBoard))
import T3.GameCallbacks.Classes (Communicator(logAction, updateBoard))

insertAtLoc :: (HasBoard m, Communicator m) => Loc -> XO -> m ()
insertAtLoc loc xo = do
  BoardManager.insertAtLoc loc xo
  logAction xo loc
  board <- getBoard
  updateBoard board
