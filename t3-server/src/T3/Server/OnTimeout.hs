module T3.Server.OnTimeout
  ( OnTimeout(..)
  ) where

import T3.Server.Milliseconds (Milliseconds)

class Monad m => OnTimeout m where
  onTimeout :: m a -> Milliseconds -> m (Maybe a)
