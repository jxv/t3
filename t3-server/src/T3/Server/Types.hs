module T3.Server.Types
  ( UserId(..)
  , HashCode(..)
  , Name(..)
  , Token(..)
  , Ticket(..)
  , GameId(..)
  , Move(..)
  , Step(..)
  , RegistryCb(..)
  , LobbyCb(..)
  , UsherCb(..)
  , GamesCb(..)
  , ResultsCb(..)
  , RegisterReq(..)
  , RegisterResp(..)
  , Creds(..)
  ) where

import Control.Monad (mzero)
import Control.Lens hiding ((.=))
import Data.Text (Text)
import Data.String (IsString)
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), Value(..), (.=), object)

newtype UserId = UserId Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON, Ord)

newtype HashCode = HashCode Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON, Ord)

newtype Name = Name Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON)

newtype Token = Token Text
  deriving (Show, Eq, IsString, FromJSON, ToJSON)

newtype Ticket = Ticket Text
  deriving (Show, Eq, IsString)

newtype GameId = GameId Text
  deriving (Show, Eq, IsString)
 
newtype Move = Move (Int,Int)
  deriving (Show, Eq)

newtype Step = Step ()
  deriving (Show, Eq)

data RegistryCb = RegistryCb
  { _registryCbHashCode :: HashCode
  , _registryCbInsertUser :: (Name, Token) -> IO (Maybe UserId)
  }

data LobbyCb = LobbyCb (IO ())

data UsherCb = UsherCb (IO ())

data GamesCb = GamesCb (IO ())

data ResultsCb = ResultsCb (IO ())

data RegisterReq = RegisterReq
  { _registerReqName :: Text
  } deriving (Show, Eq)

data Creds = Creds
  { _credsUserId :: UserId
  , _credsToken :: Token
  } deriving (Show, Eq)

data RegisterResp = RegisterResp
  { _registerRespCreds :: Creds
  } deriving (Show, Eq)

instance FromJSON RegisterReq where
  parseJSON (Object v) = RegisterReq <$> (v .: "name")
  parseJSON _ = mzero

instance ToJSON Creds where
  toJSON (Creds userId token) = object ["userId" .= userId, "token" .= token]

instance ToJSON RegisterResp where
  toJSON (RegisterResp creds) = object ["creds" .= creds]
