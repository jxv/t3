module T3.Game.BoardManager
  ( BoardManager(..)
  , isOpenLoc'
  , isOpenLoc_
  , insertAtLoc'
  , insertAtLoc_
  , getResult'
  , getResult_
  ) where

import T3.Core (Loc, XO, Result, valid, insertXO, result, Board)
import T3.Game.HasBoard (HasBoard(getBoard, putBoard))

class Monad m => BoardManager m where
  isOpenLoc :: Loc -> m Bool
  insertAtLoc :: Loc -> XO -> m ()
  getResult :: m Result

isOpenLoc' :: HasBoard m => Loc -> m Bool
isOpenLoc' loc = do
  board <- getBoard
  return $ valid loc board

isOpenLoc_ :: Loc -> Board -> Bool
isOpenLoc_ = valid

insertAtLoc' :: HasBoard m => Loc -> XO -> m ()
insertAtLoc' loc xo = do
  board <- getBoard
  putBoard $ insertXO loc xo board

insertAtLoc_ :: Loc -> XO -> Board -> Board
insertAtLoc_ = insertXO

getResult' :: HasBoard m => m Result
getResult' = do
  board <- getBoard
  return $ result board

getResult_ :: Board -> Result
getResult_ = result
