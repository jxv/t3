module T3.GameCallbacks.HasTimeoutLimit
  ( HasTimeoutLimit(..)
  ) where

import T3.GameCallbacks.Milliseconds (Milliseconds)

class Monad m => HasTimeoutLimit m where
  getTimeoutLimit :: m (Maybe Milliseconds)
