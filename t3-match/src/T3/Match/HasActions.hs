module T3.Match.HasActions
  ( HasActions(..)
  ) where

import T3.Core (Action)

class Monad m => HasActions m where
  getActions :: m [Action]
  putActions :: [Action] -> m ()
