module T3.GameCallbacks.OnTimeout
  ( OnTimeout(..)
  ) where

import T3.GameCallbacks.Milliseconds (Milliseconds)

class Monad m => OnTimeout m where
  onTimeout :: m a -> Milliseconds -> m (Maybe a)
