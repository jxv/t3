{-# LANGUAGE TemplateHaskell #-}
module T3.Server.Types
  ( Env(..)
  , AppHandler(..)
  , HasEnv(..)

  , UserId(..)
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
  , UsherCb(..)
  , GamesCb(..)
  , ResultsCb(..)
  , RegisterReq(..)
  , RegisterResp(..)
  , LobbyReq(..)
  , LobbyResp(..)
  , callback
  ) where

import Control.Monad (mzero)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import Data.String (IsString)
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), Value(..), (.=), object)
import Servant

data Env = Env
  { _envPort :: Int
  , _envLobbyCb :: LobbyCb
  , _envUsherCb :: UsherCb
  , _envGamesCb :: GamesCb
  , _envResultsCb :: ResultsCb
  , _envRegistryCb :: RegistryCb
  }

callback :: (Env -> b -> IO a) -> b -> AppHandler a
callback x i = do
  f <- asks x
  liftIO (f i)

newtype AppHandler a = AppHandler { runHandler :: ReaderT Env (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError ServantErr, MonadIO)

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
  deriving (Show, Eq, IsString, FromJSON, ToJSON)
 
newtype Move = Move (Int,Int)
  deriving (Show, Eq)

newtype Step = Step ()
  deriving (Show, Eq)

data RegistryCb = RegistryCb
  { _registryCbHashCode :: HashCode
  , _registryCbInsertUser :: (Name, Token) -> IO (Maybe UserId)
  , _registryCbGetUserById :: UserId -> IO (Maybe (Name, Token))
  }

data LobbyCb = LobbyCb 
  { _lobbyCbAddToLobby :: UserId -> IO (Maybe Ticket)
  , _lobbyCbTransferTicket :: Ticket -> IO (Maybe GameId)
  }

data UsherCb = UsherCb (IO ())

data GamesCb = GamesCb (IO ())

data ResultsCb = ResultsCb (IO ())

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

makeClassy ''Env
