module T3.Match.MatchLogger
  ( MatchLogger(..)
  ) where

import T3.Core (Result, Action, Board)

class Monad m => MatchLogger m where
  logMatch :: [Action] -> Board -> Result -> m ()
