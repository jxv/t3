module T3.Server.Control.PracticeLobby
  ( Lobby(..)
  , Registry(..)
  , UserStore(..)
  , RegistryState(..)
  , LobbyState(..)
  , Failure(..)
  , practiceLobby
  , validateUser'
  , getUser'
  , queueUser'
  ) where

import Control.Concurrent.Chan (readChan)
import Control.Applicative ((<|>))
import Control.Monad (forever, unless)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks)
import Control.Monad.Except (ExceptT, MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant

import T3.Server.Types
import T3.Server.Control.Types

class Monad m => Lobby m where
  queueUser :: UserId -> m GameStart

class Monad m => Game m where
  getStep :: GameId -> UserId -> m Step

class Monad m => Registry m where
  validateUser :: UserId -> Token -> m ()

class Monad m => UserStore m where
  getUser :: UserId -> m (Name, Token)

class Monad m => RegistryState m where
  getUserById :: UserId -> m (Maybe (Name, Token))

class Monad m => LobbyState m where
  transferUser :: UserId -> m (Maybe GameStart)

class Monad m => Failure m where
  validateUserFailure :: m a
  getUserFailure :: m a
  transferUserFailure :: m a

practiceLobby :: (Lobby m, Registry m, Game m) => LobbyReq -> m LobbyResp
practiceLobby (LobbyReq (Creds userId token)) = do
  validateUser userId token
  gameStart <- queueUser userId
  step <- getStep (_gameStartGameId gameStart) userId
  return $ LobbyResp (StepJSON step) gameStart

validateUser' :: (UserStore m, Failure m) => UserId -> Token -> m ()
validateUser' userId token = do
  (_, token') <- getUser userId
  unless (token == token') validateUserFailure

getUser' :: (RegistryState m, Failure m) => UserId -> m (Name, Token)
getUser' = try getUserFailure . getUserById

queueUser' :: (LobbyState m, Failure m) => UserId -> m GameStart
queueUser' = try transferUserFailure . transferUser

getStep' :: (MonadIO m, MonadReader Env m) => GameId -> UserId -> m Step
getStep' gameId userId = do
  findGameObject <- asks (_gamesObjectFindGame . _envGamesObject)
  mgr <- liftIO $ findGameObject gameId
  case mgr of
    Nothing -> error "Can't find Game by GameId"
    Just gr -> do
      let x = stepByUserId userId (_pX $ _gameRecordLabeled gr)
      let o = stepByUserId userId (_pO $ _gameRecordLabeled gr)
      case x <|> o of
        Nothing -> error "Can't find User in Game by UserId"
        Just recvStep -> liftIO recvStep

stepByUserId :: UserId -> LabeledGameObject -> Maybe (IO Step)
stepByUserId userId (LabeledGameObject userId' (GameObject _ g)) =
  if userId == userId'
    then Just $ readChan g
    else Nothing

instance Lobby AppHandler where
  queueUser = queueUser'

instance Registry AppHandler where
  validateUser = validateUser'

instance UserStore AppHandler where
  getUser = getUser'

instance LobbyState AppHandler where
  transferUser = callback (_lobbyObjectTransferUser . _envLobbyObject)

instance RegistryState AppHandler where
  getUserById = callback (_registryObjectGetUserById . _envRegistryObject)

instance Game AppHandler where
  getStep = getStep'

instance Failure AppHandler where
  validateUserFailure = throwError $ errMsg "Can't validate user"
  getUserFailure = throwError $ errMsg "Can't get user"
  transferUserFailure = throwError $ errMsg "Can't transfer user"

errMsg :: String -> ServantErr
errMsg msg  = ServantErr
  { errHTTPCode = 400
  , errReasonPhrase = msg
  , errBody = ""
  , errHeaders = []
  }
