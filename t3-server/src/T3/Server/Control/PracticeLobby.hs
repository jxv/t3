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
  , standbyForCall'
  , queueUser'
  ) where

import Control.Lens
import Control.Monad (forever, unless)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks)
import Control.Monad.Except (ExceptT, MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant

import T3.Server.Types
import T3.Server.Control.Types

class Monad m => Lobby m where
  queueUser :: UserId -> m Ticket
  standbyForCall :: Ticket -> m GameId

class Monad m => Registry m where
  validateUser :: UserId -> Token -> m ()

class Monad m => UserStore m where
  getUser :: UserId -> m (Name, Token)

class Monad m => RegistryState m where
  getUserById :: UserId -> m (Maybe (Name, Token))

class Monad m => LobbyState m where
  addToLobby :: UserId -> m (Maybe Ticket)
  transferTicket :: Ticket -> m (Maybe GameId)

class Monad m => Failure m where
  validateUserFailure :: m a
  getUserFailure :: m a
  addToLobbyFailure :: m a
  transferTicketFailure :: m a

practiceLobby :: (Lobby m, Registry m) => LobbyReq -> m LobbyResp
practiceLobby (LobbyReq (Creds userId token)) = do
  validateUser userId token
  ticket <- queueUser userId
  gameId <- standbyForCall ticket
  return $ LobbyResp gameId

validateUser' :: (UserStore m, Failure m) => UserId -> Token -> m ()
validateUser' userId token = do
  (_, token') <- getUser userId
  unless (token == token') validateUserFailure

getUser' :: (RegistryState m, Failure m) => UserId -> m (Name, Token)
getUser' = try getUserFailure . getUserById

standbyForCall' :: (LobbyState m, Failure m) => Ticket -> m GameId
standbyForCall' = try transferTicketFailure . transferTicket

queueUser' :: (LobbyState m, Failure m) => UserId -> m Ticket
queueUser' = try addToLobbyFailure . addToLobby

instance Lobby AppHandler where
  queueUser = queueUser'
  standbyForCall = standbyForCall'

instance Registry AppHandler where
  validateUser = validateUser'

instance UserStore AppHandler where
  getUser = getUser'

instance LobbyState AppHandler where
  addToLobby = callback (_lobbyCbAddToLobby . _envLobbyCb)
  transferTicket = callback (_lobbyCbTransferTicket . _envLobbyCb)

instance RegistryState AppHandler where
  getUserById = callback (_registryCbGetUserById . _envRegistryCb)

instance Failure AppHandler where
  validateUserFailure = throwError $ errMsg "Can't validate user"
  getUserFailure = throwError $ errMsg "Can't get user"
  addToLobbyFailure = throwError $ errMsg "Can't add to lobby"
  transferTicketFailure = throwError $ errMsg "Can't transfer ticket"

errMsg :: String -> ServantErr
errMsg msg  = ServantErr
  { errHTTPCode = 400
  , errReasonPhrase = msg
  , errBody = ""
  , errHeaders = []
  }
