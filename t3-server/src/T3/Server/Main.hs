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
import T3.Server.SharedCb (newRegistryCb', newGamesCb', newLobbyCb', newResultsCb', newGameCb')

class Monad m => SharedCb m where
  newLobbyCb :: m LobbyCb
  newGamesCb :: m GamesCb
  newResultsCb :: m ResultsCb
  newRegistryCb :: m RegistryCb
  newGameCb :: m GameCb

class Monad m => Interthread m where
  fork :: m () -> m ThreadCb

class Monad m => Threads m where
  arenaDispatcher :: LobbyCb -> GamesCb -> (GameStart -> m (ThreadCb, GameCb, GameCb)) -> m ()
  practiceDispatcher :: LobbyCb -> GamesCb -> (GameStart -> m (ThreadCb, GameCb, GameCb)) -> m ()
  game :: ResultsCb -> GamesCb -> GameStart -> GameCb -> GameCb -> m ()
  control :: LobbyCb -> GamesCb -> ResultsCb -> RegistryCb -> m ()

main :: (SharedCb m, Interthread m, Threads m) => m ()
main = do
  lobby <- newLobbyCb
  games <- newGamesCb
  results <- newResultsCb
  registry <- newRegistryCb
  void . fork $ practiceDispatcher lobby games (gameDispatch results games)
  void . fork $ arenaDispatcher lobby games (gameDispatch results games)
  control lobby games results registry

gameDispatch :: (SharedCb m, Interthread m, Threads m) => ResultsCb -> GamesCb -> GameStart -> m (ThreadCb, GameCb, GameCb)
gameDispatch results games gameStart = do
  cbA <- newGameCb
  cbB <- newGameCb
  threadCb <- fork $ game results games gameStart cbA cbB
  return (threadCb, cbA, cbB)

newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runServer :: Server a -> IO a
runServer (Server m) = m

instance Interthread Server where
  fork = fmap mkThreadCb . liftIO . forkIO . runServer
    where
      mkThreadCb :: ThreadId -> ThreadCb
      mkThreadCb thid = ThreadCb
        { _threadCbHashCode = HashCode . pack $ show thid
        , _threadCbKill = killThread thid
        }

instance SharedCb Server where
  newLobbyCb = newLobbyCb'
  newGamesCb = newGamesCb'
  newResultsCb = newResultsCb'
  newRegistryCb = newRegistryCb'
  newGameCb = newGameCb'

instance Threads Server where
  arenaDispatcher _ _ _ = return ()
  practiceDispatcher lobby games dispatch = PD.run PD.main (PD.Env lobby games (runServer . dispatch))
  game results games gameStart gameCbA gameCbB = Game.run Game.main (Game.Env results games gameStart gameCbA gameCbB)
  control lobby games results registry = Control.main (Control.Env 8080 lobby games results registry)
