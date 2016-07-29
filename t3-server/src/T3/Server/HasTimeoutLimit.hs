module T3.Server.HasTimeoutLimit
  ( HasTimeoutLimit(..)
  ) where

import T3.Server.Milliseconds (Milliseconds)

class Monad m => HasTimeoutLimit m where
  getTimeoutLimit :: m (Maybe Milliseconds)
