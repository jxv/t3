module T3.Server.Types
  ( UserId(..)
  , HashCode(..)
  , Name(..)
  , Token(..)
  , Ticket(..)
  , GameId(..)
  , Move(..)
  , Step(..)
  , Creds(..)
  , RegistryCb(..)
  , LobbyCb(..)
  , GamesCb(..)
  , ResultsCb(..)
  , RegisterReq(..)
  , RegisterResp(..)
  , LobbyReq(..)
  , LobbyResp(..)
  , try
  , callback
  , GameStart(..)
  , ThreadCb(..)
  , GameCb
  , GameRec
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Monad (mzero)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import Data.String (IsString)
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), Value(..), (.=), object)
import Servant

import T3.Core (Loc)
import T3.Game.Types (Step)

try :: Monad m => m a -> m (Maybe a) -> m a
try err f = do
  mx <- f
  case mx of
    Nothing -> err
    Just x -> return x

callback :: (MonadReader r m, MonadIO m) => (r -> b -> IO a) -> b -> m a
callback x i = do
  f <- asks x
  liftIO (f i)

newtype UserId = UserId Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON, Ord)

newtype HashCode = HashCode Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON, Ord)

newtype Name = Name Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON)

newtype Token = Token Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON)

newtype Ticket = Ticket Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON)

newtype GameId = GameId Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON, Ord)

newtype Move = Move (Int,Int)
  deriving (Show, Eq)

data GameStart = GameStart
  { _gameStartGameId :: GameId
  , _gameStartX :: UserId
  , _gameStartO :: UserId
  } deriving (Show, Eq)

data RegistryCb = RegistryCb
  { _registryCbHashCode :: HashCode
  , _registryCbInsertUser :: (Name, Token) -> IO (Maybe UserId)
  , _registryCbGetUserById :: UserId -> IO (Maybe (Name, Token))
  }

data LobbyCb = LobbyCb
  { _lobbyCbHashCode :: HashCode
  , _lobbyCbTransferUser :: UserId -> IO (Maybe GameId)
  , _lobbyCbDequeueUser :: GameId -> IO (Maybe UserId)
  , _lobbyCbAnnounceGame :: GameId -> IO ()
  }

type GameCb = (Chan Loc, Chan Step)
type GameRec = (ThreadCb, (UserId, GameCb), (UserId, GameCb))

data GamesCb = GamesCb
  { _gamesCbHashCode :: HashCode
  , _gamesCbInsertGame :: (GameId, GameRec) -> IO ()
  }

data ResultsCb = ResultsCb (IO ())

data ThreadCb = ThreadCb
  { _threadCbHashCode :: HashCode
  , _threadCbKill :: IO ()
  }

data Creds = Creds
  { _credsUserId :: UserId
  , _credsToken :: Token
  } deriving (Show, Eq)

data RegisterReq = RegisterReq
  { _registerReqName :: Name
  } deriving (Show, Eq)

data RegisterResp = RegisterResp
  { _registerRespCreds :: Creds
  } deriving (Show, Eq)

data LobbyReq = LobbyReq
  { _lobbyReqCreds :: Creds
  } deriving (Show, Eq)

data LobbyResp = LobbyResp
  { _lobbyRespGameId :: GameId
  } deriving (Show, Eq)

instance FromJSON RegisterReq where
  parseJSON (Object v) = RegisterReq <$> v .: "name"
  parseJSON _ = mzero

instance FromJSON Creds where
  parseJSON (Object v) = Creds <$> v .: "userId" <*> v .: "token"
  parseJSON _ = mzero

instance ToJSON Creds where
  toJSON (Creds userId token) = object ["userId" .= userId, "token" .= token]

instance ToJSON RegisterResp where
  toJSON (RegisterResp creds) = object ["creds" .= creds]

instance FromJSON LobbyReq where
  parseJSON (Object v) = LobbyReq <$> v .: "creds"
  parseJSON _ = mzero

instance ToJSON LobbyResp where
  toJSON (LobbyResp gameId) = object ["gameId" .= gameId]
