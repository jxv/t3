module T3.Game.BoardManagerImpl
  ( isOpenLoc
  , insertAtLoc
  , getResult
  ) where

import T3.Core (Loc, XO, Result, valid, insertXO, result)
import T3.Game.Parts (HasBoard(getBoard, putBoard))

isOpenLoc :: HasBoard m => Loc -> m Bool
isOpenLoc loc = do
  board <- getBoard
  return $ valid loc board

insertAtLoc :: HasBoard m => Loc -> XO -> m ()
insertAtLoc loc xo = do
  board <- getBoard
  putBoard $ insertXO loc xo board

getResult :: HasBoard m => m Result
getResult = do
  board <- getBoard
  return $ result board
