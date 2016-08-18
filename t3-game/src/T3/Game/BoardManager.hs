module T3.Game.BoardManager
  ( BoardManager(..)
  ) where

import T3.Core (Loc, XO, Result)

class Monad m => BoardManager m where
  isOpenLoc :: Loc -> m Bool
  insertAtLoc :: Loc -> XO -> m ()
  getResult :: m Result
