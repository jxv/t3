module T3.GameCallbacks.HasActions
  ( HasActions(..)
  ) where

import T3.Core (Action)

class Monad m => HasActions m where
  getActions :: m [Action]
  putActions :: [Action] -> m ()
