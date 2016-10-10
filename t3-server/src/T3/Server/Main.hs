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
  arenaDispatcher :: LobbyObject -> GamesObject -> (GameStart -> m GameEntry) -> m ()
  practiceDispatcher :: LobbyObject -> GamesObject -> (GameStart -> m GameEntry) -> m ()
  game :: ResultsObject -> GamesObject -> GameStart -> Pair GameObject -> m ()
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

gameDispatch :: (SharedObject m, Interthread m, Threads m) => ResultsObject -> GamesObject -> GameStart -> m GameEntry
gameDispatch results games gs = do
  objs@(Pair x o) <- Pair <$> newGameObject <*> newGameObject
  th <- fork $ game results games gs objs
  return $ GameEntry th (Pair x o)

newtype Server a = Server { runServer :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

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
  game results games gameStart gameObjs = Game.run Game.main (Game.Env results games gameStart gameObjs)
  control lobby games results registry = Control.main (Control.Env 8080 lobby games results registry)
