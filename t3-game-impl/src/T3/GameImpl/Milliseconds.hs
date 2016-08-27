module T3.GameImpl.Milliseconds
  ( Milliseconds(..)
  , Delayer(..)
  ) where

import Control.Monad.Conc.Class (threadDelay)

newtype Milliseconds = Milliseconds Int
  deriving (Show, Eq, Ord, Num)

class Monad m => Delayer m where
  delay :: Milliseconds -> m ()

instance Delayer IO where
  delay (Milliseconds ms) = threadDelay (scaleFromNano * ms)
    where
      scaleFromNano = 1000
