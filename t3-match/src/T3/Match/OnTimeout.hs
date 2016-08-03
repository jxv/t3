module T3.Match.OnTimeout
  ( OnTimeout(..)
  ) where

import T3.Match.Milliseconds (Milliseconds)

class Monad m => OnTimeout m where
  onTimeout :: m a -> Milliseconds -> m (Maybe a)
