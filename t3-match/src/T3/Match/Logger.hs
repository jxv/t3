module T3.Match.Logger
  ( Logger(..)
  ) where

import T3.Core (Result, Action, Board)

class Monad m => Logger m where
  logIt :: [Action] -> Board -> Result -> m ()
