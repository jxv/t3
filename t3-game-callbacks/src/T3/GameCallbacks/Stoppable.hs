module T3.GameCallbacks.Stoppable
  ( Stoppable(..)
  ) where

class Monad m => Stoppable m where
  stop :: m a
