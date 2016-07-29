module T3.Server.Stoppable
  ( Stoppable(..)
  ) where

class Monad m => Stoppable m where
  stop :: m a 
