module T3.Server.HasMatchState
  ( HasMatchState(..)
  ) where

import T3.Core (Action, Board)

class Monad m => HasMatchState m where
  getBoard :: m Board
  putBoard :: Board -> m ()
  getActions :: m [Action]
  appendAction :: Action -> m ()
