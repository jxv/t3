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
  , RegistryObject(..)
  , LobbyObject(..)
  , GamesObject(..)
  , ResultsObject(..)
  , RegisterReq(..)
  , RegisterResp(..)
  , LobbyReq(..)
  , LobbyResp(..)
  , PlayReq(..)
  , PlayResp(..)
  , GameStart(..)
  , ThreadObject(..)
  , GameObject(..)
  , GameRecord(..)
  , GameEntry(..)
  , LabeledGameObject(..)
  , FinalJSON(..)
  , StepJSON(..)
  , Pair(..)
  , byUser
  , try
  , callback
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

import T3.Core (Loc, XO(..))
import T3.Game.Types (Step(..), Final(..))


byUser :: a -> a -> XO -> a
byUser x _ X = x
byUser _ o O = o

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

data Pair a = Pair
  { _pX :: !a
  , _pO :: !a
  } deriving (Show, Eq)

data GameStart = GameStart
  { _gameStartGameId :: !GameId
  , _gameStartX :: !UserId
  , _gameStartO :: !UserId
  } deriving (Show, Eq)

data RegistryObject = RegistryObject
  { _registryObjectHashCode :: !HashCode
  , _registryObjectInsertUser :: !((Name, Token) -> IO (Maybe UserId))
  , _registryObjectGetUserById :: !(UserId -> IO (Maybe (Name, Token)))
  }

data LobbyObject = LobbyObject
  { _lobbyObjectHashCode :: !HashCode
  , _lobbyObjectTransferUser :: !(UserId -> IO (Maybe GameStart))
  , _lobbyObjectDequeueUser :: !(GameId -> IO (Maybe UserId))
  , _lobbyObjectAnnounceGame :: !(GameStart -> IO ())
  }

data GameEntry = GameEntry
  { _gameEntryThreadObject :: ThreadObject
  , _gameEntryGameObjects :: Pair GameObject
  }

data GameObject = GameObject
  { _gameObjectLoc :: Chan Loc
  , _gameObjectStep :: Chan Step
  }

data GameRecord = GameRecord
  { _gameRecordThreadObject :: ThreadObject
  , _gameRecordLabeled :: Pair LabeledGameObject
  }

data LabeledGameObject = LabeledGameObject
  { _labeledGameObjectUserId :: UserId
  , _labeledGameObjectGameObject :: GameObject
  }

data GamesObject = GamesObject
  { _gamesObjectHashCode :: !HashCode
  , _gamesObjectInsertGame :: !((GameId, GameRecord) -> IO ())
  , _gamesObjectFindGame :: !(GameId -> IO (Maybe GameRecord))
  , _gamesObjectRemoveGame :: !(GameId -> IO ())
  }

data ResultsObject = ResultsObject { wahhh :: IO () }

data ThreadObject = ThreadObject
  { _threadObjectHashCode :: !HashCode
  , _threadObjectKill :: !(IO ())
  }

data Creds = Creds
  { _credsUserId :: !UserId
  , _credsToken :: !Token
  } deriving (Show, Eq)

data RegisterReq = RegisterReq
  { _registerReqName :: !Name
  } deriving (Show, Eq)

data RegisterResp = RegisterResp
  { _registerRespCreds :: !Creds
  } deriving (Show, Eq)

data LobbyReq = LobbyReq
  { _lobbyReqCreds :: !Creds
  } deriving (Show, Eq)

data LobbyResp = LobbyResp
  { _lobbyRespStep :: !StepJSON
  , _lobbyRespGameStart :: !GameStart
  } deriving (Show, Eq)

data PlayReq = PlayReq
  { _playReqCreds :: !Creds
  , _playReqGameId :: !GameId
  , _playReqLoc :: !Loc
  } deriving (Show, Eq)

data PlayResp = PlayResp
  { _playRespStep :: !StepJSON
  } deriving (Show, Eq)


newtype StepJSON = StepJSON Step
  deriving (Show, Eq)

newtype FinalJSON = FinalJSON Final
  deriving (Show, Eq)

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
  toJSON (LobbyResp step gameStart) = object ["step" .= step, "start" .= gameStart]

instance FromJSON PlayReq where
  parseJSON (Object v) = PlayReq <$> v .: "creds" <*> v .: "gameId" <*> v .: "loc"
  parseJSON _ = mzero

instance ToJSON PlayResp where
  toJSON (PlayResp step) = object ["step" .= step]

instance ToJSON StepJSON where
  toJSON (StepJSON step) = object ["board" .= (_stepBoard step), "final" .= (FinalJSON <$> _stepFinal step)]

instance ToJSON FinalJSON where
  toJSON (FinalJSON final) = case final of
    Won -> String "Won"
    WonByDQ -> String "WonByDQ"
    Loss -> String "Loss"
    LossByDQ -> String "LossByDQ"
    Tied -> String "Tied"

instance ToJSON GameStart where
  toJSON gs = object ["gameId" .= (_gameStartGameId gs), "x" .= (_gameStartX gs), "o" .= (_gameStartO gs)]
