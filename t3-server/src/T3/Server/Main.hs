module T3.Server.Main
  ( main
  , runServer
  ) where

import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import qualified T3.Server.Control as Control
import T3.Server.Types (LobbyCb, GamesCb, ResultsCb, RegistryCb)
import T3.Server.SharedCb (newRegistryCb')

class Monad m => SharedCb m where
  newLobbyCb :: m LobbyCb
  newGamesCb :: m GamesCb
  newResultsCb :: m ResultsCb
  newRegistryCb :: m RegistryCb

class Monad m => Interthread m where
  fork :: m () -> m ()

class Monad m => Threads m where
  arenaDispatcher :: LobbyCb -> GamesCb -> m () -> m ()
  practiceDispatcher :: LobbyCb -> GamesCb -> m () -> m ()
  game :: ResultsCb -> m ()
  control :: LobbyCb -> GamesCb -> ResultsCb -> RegistryCb -> m ()

main :: (SharedCb m, Interthread m, Threads m) => m ()
main = do
  lobby <- newLobbyCb
  games <- newGamesCb
  results <- newResultsCb
  registry <- newRegistryCb
  fork $ practiceDispatcher lobby games (fork $ game results)
  fork $ arenaDispatcher lobby games (fork $ game results)
  control lobby games results registry

newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runServer :: Server a -> IO a
runServer (Server m) = m

instance Interthread Server where
  fork = void . liftIO . forkIO . runServer

instance SharedCb Server where
  newLobbyCb = return undefined
  newGamesCb = return undefined
  newResultsCb = return undefined
  newRegistryCb = newRegistryCb'

instance Threads Server where
  arenaDispatcher _ _ _ = return ()
  practiceDispatcher _ _ _ = return ()
  game _ = return ()
  control lobby games results registry = Control.main $ Control.Env 8080 lobby games results registry
