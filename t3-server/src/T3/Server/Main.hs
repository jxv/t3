module T3.Server.Main
  ( main
  , runServer
  ) where

import qualified Data.Map as Map
import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)
import Data.Text (pack)

import qualified T3.Server.Control.Monad as Control
import qualified T3.Server.PracticeDispatcher as PD
import qualified T3.Server.Game as Game
import T3.Server.Types
import T3.Server.Shared.GameObject (newGameObject')
import T3.Server.Shared.GamesObject (newGamesObject')
import T3.Server.Shared.LobbyObject (newLobbyObject')
import T3.Server.Shared.RegistryObject (newRegistryObject')
import T3.Server.Shared.ResultsObject (newResultsObject')

class Monad m => SharedObject m where
  newLobbyObject :: m LobbyObject
  newGamesObject :: m GamesObject
  newResultsObject :: m ResultsObject
  newRegistryObject :: m RegistryObject
  newGameObject :: m GameObject

class Monad m => Interthread m where
  fork :: m () -> m ThreadObject

class Monad m => Threads m where
  arenaDispatcher :: LobbyObject -> GamesObject -> (GameStart -> m (ThreadObject, GameObject, GameObject)) -> m ()
  practiceDispatcher :: LobbyObject -> GamesObject -> (GameStart -> m (ThreadObject, GameObject, GameObject)) -> m ()
  game :: ResultsObject -> GamesObject -> GameStart -> GameObject -> GameObject -> m ()
  control :: LobbyObject -> GamesObject -> ResultsObject -> RegistryObject -> m ()

main :: (SharedObject m, Interthread m, Threads m) => m ()
main = do
  lobby <- newLobbyObject
  games <- newGamesObject
  results <- newResultsObject
  registry <- newRegistryObject
  void . fork $ practiceDispatcher lobby games (gameDispatch results games)
  void . fork $ arenaDispatcher lobby games (gameDispatch results games)
  control lobby games results registry

gameDispatch :: (SharedObject m, Interthread m, Threads m) => ResultsObject -> GamesObject -> GameStart -> m (ThreadObject, GameObject, GameObject)
gameDispatch results games gameStart = do
  objectX <- newGameObject
  objectO <- newGameObject
  threadObject <- fork $ game results games gameStart objectX objectO
  return (threadObject, objectX, objectO)

newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runServer :: Server a -> IO a
runServer (Server m) = m

instance Interthread Server where
  fork = fmap mkThreadObject . liftIO . forkIO . runServer
    where
      mkThreadObject :: ThreadId -> ThreadObject
      mkThreadObject thid = ThreadObject
        { _threadObjectHashCode = HashCode . pack $ show thid
        , _threadObjectKill = killThread thid
        }

instance SharedObject Server where
  newLobbyObject = newLobbyObject'
  newGamesObject = newGamesObject'
  newResultsObject = newResultsObject'
  newRegistryObject = newRegistryObject'
  newGameObject = newGameObject'

instance Threads Server where
  arenaDispatcher _ _ _ = return ()
  practiceDispatcher lobby games dispatch = PD.run PD.main (PD.Env lobby games (runServer . dispatch))
  game results games gameStart gameObjectX gameObjectO = Game.run Game.main (Game.Env results games gameStart gameObjectX gameObjectO)
  control lobby games results registry = Control.main (Control.Env 8080 lobby games results registry)
