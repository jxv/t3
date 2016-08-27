module T3.Match.BoardManagerImpl
  ( insertAtLoc
  ) where

import qualified T3.Game.BoardManagerImpl as BoardManager (insertAtLoc)
import T3.Core (Loc, XO)
import T3.Game.Parts (HasBoard(getBoard))
import T3.Match.Parts (Communicator(logAction, updateBoard))

insertAtLoc :: (HasBoard m, Communicator m) => Loc -> XO -> m ()
insertAtLoc loc xo = do
  BoardManager.insertAtLoc loc xo
  logAction xo loc
  board <- getBoard
  updateBoard board
