module T3.Match.HasTimeoutLimit
  ( HasTimeoutLimit(..)
  ) where

import T3.Match.Milliseconds (Milliseconds)

class Monad m => HasTimeoutLimit m where
  getTimeoutLimit :: m (Maybe Milliseconds)
