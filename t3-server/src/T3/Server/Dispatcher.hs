module T3.Server.Dispatcher
  ( Env(..)
  , run
  , practiceMain
  , practiceStep
  , arenaMain
  , arenaStep
  , botId
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask), asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types
import T3.Server.Gen

class Monad m => LobbyControl m where
  popUser :: GameId -> m UserId
  announceGame :: GameStart -> m ()

class Monad m => Gen m where
  genGameId :: m GameId

class Monad m => GameDispatch m where
  dispatchGame :: GameStart -> m ()

class Monad m => Dispatch m where
  dispatch :: GameStart -> m GameEntry

class Monad m => Games m where
  insertGame :: (GameId, GameRecord) -> m ()

class Monad m => Lobby m where
  dequeueUser :: GameId -> m (Maybe UserId)

class Monad m => Delay m where
  delay :: Int -> m ()

botId :: UserId
botId = "bot"

practiceMain :: (LobbyControl m, GameDispatch m, Gen m) => m ()
practiceMain = forever practiceStep

practiceStep :: (LobbyControl m, GameDispatch m, Gen m) => m ()
practiceStep = do
  gameId <- genGameId
  userId <- popUser gameId
  let gs = (GameStart gameId userId botId)
  dispatchGame gs
  announceGame gs

arenaMain :: (LobbyControl m, GameDispatch m, Gen m) => m ()
arenaMain = forever arenaStep

arenaStep :: (LobbyControl m, GameDispatch m, Gen m) => m ()
arenaStep = do
  gameId <- genGameId
  uidX <- popUser gameId
  uidO <- popUser gameId
  let gs = (GameStart gameId uidX uidO)
  dispatchGame gs
  announceGame gs

dispatchGame' :: (Dispatch m, Games m) => GameStart -> m ()
dispatchGame' gs@(GameStart gameId userX userO) = do
  (GameEntry threadObject (Pair gameObjectX gameObjectO)) <- dispatch gs
  insertGame (gameId, GameRecord threadObject $ Pair (LabeledGameObject userX gameObjectX) (LabeledGameObject userO gameObjectO))

popUser' :: (LobbyControl m, Lobby m, Delay m) => GameId -> m UserId
popUser' gameId = do
  mUserId <- dequeueUser gameId
  case mUserId of
    Nothing -> delay 1 >> popUser gameId
    Just userId -> return userId

data Env = Env
  { _envLobbyObject :: LobbyObject
  , _envGamesObject :: GamesObject
  , _envDispatch :: GameStart -> IO GameEntry
  }

newtype PracticeDispatcher a = PracticeDispatcher (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

run :: MonadIO m => PracticeDispatcher a -> Env -> m a
run (PracticeDispatcher m) env = liftIO $ runReaderT m env

instance LobbyControl PracticeDispatcher where
  popUser = popUser'
  announceGame = callback (_lobbyObjectAnnounceGame . _envLobbyObject)

instance GameDispatch PracticeDispatcher where
  dispatchGame = dispatchGame'

instance Gen PracticeDispatcher where
  genGameId = genGameId'

instance Dispatch PracticeDispatcher where
  dispatch = callback _envDispatch

instance Games PracticeDispatcher where
  insertGame = callback (_gamesObjectInsertGame . _envGamesObject)

instance Lobby PracticeDispatcher where
  dequeueUser = callback (_lobbyObjectDequeueUser . _envLobbyObject)

instance Delay PracticeDispatcher where
  delay secs = liftIO $ threadDelay (1000000 * secs)
