module T3.Server.OnTimeout
  ( OnTimeout(..)
  ) where

class Monad m => OnTimeout m where
  onTimeout :: m a -> m () -> m a
