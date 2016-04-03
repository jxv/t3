module Control.Monad.Conc.ClassTmp
  ( MonadConc(..)
  ) where

import qualified Control.Concurrent.Async as IO
import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM as IO

class Monad m => MonadConc m where
  threadDelay :: Int -> m ()
  race :: m a -> m b -> m (Either a b)
  fork :: m () -> m IO.ThreadId
  newChan :: m (IO.Chan a)
  readChan :: IO.Chan a -> m a
  writeChan :: IO.Chan a -> a -> m ()
  killThread :: IO.ThreadId -> m ()
  newTVarIO :: a -> m (IO.TVar a)
  atomically :: IO.STM a -> m a
  newEmptyMVar :: m (IO.MVar a)
  putMVar :: IO.MVar a -> a -> m ()
  takeMVar :: IO.MVar a -> m a

instance MonadConc IO where
  threadDelay = IO.threadDelay
  race = IO.race
  fork = IO.forkIO
  newChan = IO.newChan
  readChan = IO.readChan
  writeChan = IO.writeChan
  killThread = IO.killThread
  newTVarIO = IO.newTVarIO
  atomically = IO.atomically
  newEmptyMVar = IO.newEmptyMVar
  putMVar = IO.putMVar
  takeMVar = IO.takeMVar
