module T3.Game.Parts
  ( BoardManager(..)
  , HasBoard(..)
  , Control(..)
  ) where

import T3.Core (Loc, XO, Result, Board)

import T3.Game.Types

class Monad m => BoardManager m where
  isOpenLoc :: Loc -> m Bool
  insertAtLoc :: Loc -> XO -> m ()
  getResult :: m Result

class Monad m => HasBoard m where
  getBoard :: m Board
  putBoard :: Board -> m ()

class Monad m => Control m where
  move :: XO -> m Loc
  forfeit :: Win XO -> Lose XO -> m ()
  end :: Win XO -> Lose XO -> m ()
  tie :: m ()
