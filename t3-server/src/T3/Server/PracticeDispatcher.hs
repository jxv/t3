{-# LANGUAGE TemplateHaskell #-}
module T3.Server.PracticeDispatcher
  ( Env(..)
  , run
  , main
  , step
  ) where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask), asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types
import T3.Server.Gen

class Monad m => Lobby m where
  dequeueUser :: GameId -> m UserId
  announceGame :: GameId -> m ()

class Monad m => Gen m where
  genGameId :: m GameId

class Monad m => GameDispatch m where
  dispatchGame :: GameStart -> m ()

class Monad m => Dispatch m where
  dispatch :: GameStart -> m ThreadCb

class Monad m => GamesState m where
  insertGame :: (GameId, ThreadCb) -> m ()

botId :: UserId
botId = "bot"

main :: (Lobby m, GameDispatch m, Gen m) => m ()
main = forever step

step :: (Lobby m, GameDispatch m, Gen m) => m ()
step = do
  gameId <- genGameId
  userId <- dequeueUser gameId
  dispatchGame (GameStart gameId userId botId)
  announceGame gameId

dispatchGame' :: (Dispatch m, GamesState m) => GameStart -> m ()
dispatchGame' gs@(GameStart gameId userA userB) = do
  threadCb <- dispatch gs
  insertGame (gameId, threadCb)

data Env = Env
  { _envLobbyCb :: LobbyCb
  , _envGamesCb :: GamesCb
  , _envDispatch :: GameStart -> IO ThreadCb
  }

newtype PracticeDispatcher a = PracticeDispatcher (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

makeClassy ''Env

run :: MonadIO m => PracticeDispatcher a -> Env -> m a
run (PracticeDispatcher m) env = liftIO $ runReaderT m env

instance Lobby PracticeDispatcher where
  dequeueUser = undefined
  announceGame = undefined

instance GameDispatch PracticeDispatcher where
  dispatchGame = dispatchGame'

instance Gen PracticeDispatcher where
  genGameId = genGameId'

instance Dispatch PracticeDispatcher where
  dispatch = callback _envDispatch

instance GamesState PracticeDispatcher where
  insertGame = callback (_gamesCbInsertGame . _envGamesCb)
