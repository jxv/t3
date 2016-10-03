module T3.Server.Main
  ( main
  , runServer
  ) where

import qualified Data.Map as Map
import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)
import Data.Text (pack)

import qualified T3.Server.Control as Control
import qualified T3.Server.PracticeDispatcher as PD
import T3.Server.Types
import T3.Server.SharedCb (newRegistryCb')

class Monad m => SharedCb m where
  newLobbyCb :: m LobbyCb
  newGamesCb :: m GamesCb
  newResultsCb :: m ResultsCb
  newRegistryCb :: m RegistryCb

class Monad m => Interthread m where
  fork :: m () -> m ThreadCb

class Monad m => Threads m where
  arenaDispatcher :: LobbyCb -> GamesCb -> m ThreadCb -> m ()
  practiceDispatcher :: LobbyCb -> GamesCb -> (GameStart -> m ThreadCb) -> m ()
  game :: ResultsCb -> GameStart -> m ()
  control :: LobbyCb -> GamesCb -> ResultsCb -> RegistryCb -> m ()

main :: (SharedCb m, Interthread m, Threads m) => m ()
main = do
  lobby <- newLobbyCb
  games <- newGamesCb
  results <- newResultsCb
  registry <- newRegistryCb
  void . fork $ practiceDispatcher lobby games (fork . game results)
  void . fork $ arenaDispatcher lobby games (fork $ game results undefined)
  control lobby games results registry

newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runServer :: Server a -> IO a
runServer (Server m) = m

instance Interthread Server where
  fork = fmap mkThreadCb . liftIO . forkIO . runServer

mkThreadCb :: ThreadId -> ThreadCb
mkThreadCb thid = ThreadCb
  { _threadCbHashCode = HashCode . pack $ show thid
  , _threadCbKill = killThread thid
  }

instance SharedCb Server where
  newLobbyCb = return undefined
  newGamesCb = return undefined
  newResultsCb = return undefined
  newRegistryCb = newRegistryCb'

instance Threads Server where
  arenaDispatcher _ _ _ = return ()
  practiceDispatcher lobby games dispatch = PD.run PD.main (PD.Env lobby games (runServer . dispatch))
  game _ _ = return ()
  control lobby games results registry = Control.main (Control.Env 8080 lobby games results registry)
