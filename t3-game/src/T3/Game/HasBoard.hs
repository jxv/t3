module T3.Game.HasBoard
  ( HasBoard(..)
  ) where

import T3.Core (Board)

class Monad m => HasBoard m where
  getBoard :: m Board
  putBoard :: Board -> m ()
