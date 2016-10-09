module T3.Server.PracticeDispatcher
  ( Env(..)
  , run
  , main
  , step
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask), asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types
import T3.Server.Gen

class Monad m => Lobby m where
  popUser :: GameId -> m UserId
  announceGame :: GameId -> m ()

class Monad m => Gen m where
  genGameId :: m GameId

class Monad m => GameDispatch m where
  dispatchGame :: GameStart -> m ()

class Monad m => Dispatch m where
  dispatch :: GameStart -> m (ThreadCb, GameCb, GameCb)

class Monad m => GamesCb' m where
  insertGame :: (GameId, GameRec) -> m ()

class Monad m => LobbyCb' m where
  dequeueUser :: GameId -> m (Maybe UserId)

class Monad m => Delay m where
  delay :: Int -> m ()

botId :: UserId
botId = "bot"

main :: (Lobby m, GameDispatch m, Gen m) => m ()
main = forever step

step :: (Lobby m, GameDispatch m, Gen m) => m ()
step = do
  gameId <- genGameId
  userId <- popUser gameId
  dispatchGame (GameStart gameId userId botId)
  announceGame gameId

dispatchGame' :: (Dispatch m, GamesCb' m) => GameStart -> m ()
dispatchGame' gs@(GameStart gameId userX userO) = do
  (threadCb, gameCbX, gameCbO) <- dispatch gs
  insertGame (gameId, (threadCb, (userX, gameCbX), (userO, gameCbO)))

popUser' :: (Lobby m, LobbyCb' m, Delay m) => GameId -> m UserId
popUser' gameId = do
  mUserId <- dequeueUser gameId
  case mUserId of
    Nothing -> delay 1 >> popUser gameId
    Just userId -> return userId

data Env = Env
  { _envLobbyCb :: LobbyCb
  , _envGamesCb :: GamesCb
  , _envDispatch :: GameStart -> IO (ThreadCb, GameCb, GameCb)
  }

newtype PracticeDispatcher a = PracticeDispatcher (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

run :: MonadIO m => PracticeDispatcher a -> Env -> m a
run (PracticeDispatcher m) env = liftIO $ runReaderT m env

instance Lobby PracticeDispatcher where
  popUser = popUser'
  announceGame = callback (_lobbyCbAnnounceGame . _envLobbyCb)

instance GameDispatch PracticeDispatcher where
  dispatchGame = dispatchGame'

instance Gen PracticeDispatcher where
  genGameId = genGameId'

instance Dispatch PracticeDispatcher where
  dispatch = callback _envDispatch

instance GamesCb' PracticeDispatcher where
  insertGame = callback (_gamesCbInsertGame . _envGamesCb)

instance LobbyCb' PracticeDispatcher where
  dequeueUser = callback (_lobbyCbDequeueUser . _envLobbyCb)

instance Delay PracticeDispatcher where
  delay secs = liftIO $ threadDelay (1000000 * secs)
