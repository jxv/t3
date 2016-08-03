module T3.Match.Stoppable
  ( Stoppable(..)
  ) where

class Monad m => Stoppable m where
  stop :: m a 
