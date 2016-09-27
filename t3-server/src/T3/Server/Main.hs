module T3.Server.Main
  ( main
  , runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import T3.Server.Types
  ( UserId
  , LobbyCb(..)
  , UsherCb(..)
  , GamesCb(..)
  , ResultsCb(..)
  , RegistryCb(..)
  )

class Monad m => SharedCb m where
  newLobbyCb :: m (LobbyCb m)
  newUsherCb :: m (UsherCb m)
  newGamesCb :: m (GamesCb m)
  newResultsCb :: m (ResultsCb m)
  newRegistryCb :: m (RegistryCb m)

class Monad m => Interthread m where
  fork :: m () -> m ()

class Monad m => Threads m where
  arenaDispatcher :: LobbyCb m -> UsherCb m -> GamesCb m -> m () -> m ()
  practiceDispatcher :: LobbyCb m -> UsherCb m -> GamesCb m -> m () -> m ()
  game :: ResultsCb m -> m ()
  control :: LobbyCb m -> UsherCb m -> GamesCb m -> ResultsCb m -> RegistryCb m -> m ()

main :: (SharedCb m, Interthread m, Threads m) => m ()
main = do
  lobby <- newLobbyCb
  usher <- newUsherCb
  games <- newGamesCb
  results <- newResultsCb
  registry <- newRegistryCb
  fork $ practiceDispatcher lobby usher games (fork $ game results)
  fork $ arenaDispatcher lobby usher games (fork $ game results)
  control lobby usher games results registry

newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runServer :: Server a -> IO a
runServer (Server m) = m

instance Interthread Server where
  fork = void . liftIO . forkIO . runServer

instance SharedCb Server where
  newLobbyCb = return undefined
  newUsherCb = return undefined
  newGamesCb = return undefined
  newResultsCb = return undefined
  newRegistryCb = return undefined

instance Threads Server where
  arenaDispatcher lobby usher games m = return ()
  practiceDispatcher lobby usher games m = return ()
  game results = return ()
  control lobby usher games results registry = return ()
