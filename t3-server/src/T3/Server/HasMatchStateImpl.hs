module T3.Server.HasMatchStateImpl
  ( MatchState(..)
  , getBoard
  , putBoard
  , getActions
  , appendAction
  ) where

import Control.Monad.State (MonadState(..), gets)

import T3.Core (Action, Board)

data MatchState = MatchState
  { _matchStateBoard :: Board
  , _matchStateActions :: [Action]
  } deriving (Show, Eq)

getBoard :: MonadState MatchState m => m Board
getBoard = gets _matchStateBoard

putBoard :: MonadState MatchState m => Board -> m ()
putBoard board = do
  matchState <- get
  put matchState{ _matchStateBoard = board }

getActions :: MonadState MatchState m => m [Action]
getActions = gets _matchStateActions

appendAction :: MonadState MatchState m => Action -> m ()
appendAction action = do
  matchState <- get
  put matchState{ _matchStateActions = _matchStateActions matchState ++ [action] }
