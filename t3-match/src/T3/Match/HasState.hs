module T3.Match.HasState
  ( HasState(..)
  ) where

import T3.Core (Action, Board)

class Monad m => HasState m where
  getBoard :: m Board
  putBoard :: Board -> m ()
  getActions :: m [Action]
  putActions :: [Action] -> m ()
