module T3.Match.Milliseconds
  ( Milliseconds(..)
  , delay
  ) where

import Control.Monad.Conc.Class (threadDelay)

newtype Milliseconds = Milliseconds Int
  deriving (Show, Eq, Ord, Num)

delay :: Milliseconds -> IO ()
delay (Milliseconds ms) = threadDelay (scaleFromNano * ms)
  where
    scaleFromNano = 1000
