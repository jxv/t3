module T3.Server.Main
  ( main
  , runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import T3.Server.Types (UserId)

type Lobby = ()

type Usher = ()

type Games = ()

type Results = ()

type Registry = ()

class Monad m => SharedState m where
  newLobby :: m Lobby
  newUsher :: m Usher
  newGames :: m Games
  newResults :: m Results
  newRegistry :: m Registry

class Monad m => Interthread m where
  fork :: m () -> m ()

class Monad m => Threads m where
  arenaDispatcher :: Lobby -> Usher -> Games -> m () -> m ()
  practiceDispatcher :: Lobby -> Usher -> Games -> m () -> m ()
  game :: Results -> m ()
  control :: Lobby -> Usher -> Games -> Results -> Registry -> m ()

main :: (SharedState m, Interthread m, Threads m) => m ()
main = do
  lobby <- newLobby
  usher <- newUsher
  games <- newGames
  results <- newResults
  registry <- newRegistry
  fork $ practiceDispatcher lobby usher games (fork $ game results)
  fork $ arenaDispatcher lobby usher games (fork $ game results)
  control lobby usher games results registry

newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runServer :: Server a -> IO a
runServer (Server m) = m

instance Interthread Server where
  fork = void . liftIO . forkIO . runServer

instance SharedState Server where
  newLobby = return ()
  newUsher = return ()
  newGames = return ()
  newResults = return ()
  newRegistry = return ()

instance Threads Server where
  arenaDispatcher lobby usher games m = return ()
  practiceDispatcher lobby usher games m = return ()
  game results = return ()
  control lobby usher games results registry = return ()
