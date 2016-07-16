{-# LANGUAGE DeriveGeneric #-}
module T3.Server
  ( RegisterRequest(..)
  , RegisterResponse(..)
  , StartRequest(..)
  , StartResponse(..)
  , PlayRequest(..)
  , PlayResponse(..)
  , MatchId(..)
  , MatchToken(..)
  , UserKey(..)
  , UserName(..)
  , UserCreds(..)
  , GameState(..)
  , UserConfig(..)
  , ServerEsque(..)
  , Step(..)
  , Users(..)
  , Final(..)
  , Seconds(..)
  , MatchInfo(..)
  , Callback
  , StartCallback
  , UserInit
  ) where

import GHC.Generics
import Control.Monad (mzero)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result, Action(..), dropPrefixP, dropPrefixJ)

type Callback m = Step -> m ()

type StartCallback m = MatchInfo -> Users -> Step -> m ()

type UserInit m = (Callback m, m (Loc, Callback m))

newtype Seconds = Seconds Int
  deriving (Num, Show, Eq, Ord, Enum)

newtype UserName = UserName { getUserName :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype MatchId = MatchId { getMatchId :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype MatchToken = MatchToken { getMatchToken :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data Users = Users
  { _uX :: UserName
  , _uO :: UserName
  } deriving (Show, Eq, Generic)

data Step = Step
  { _stepBoard :: Board
  , _stepFinal :: Maybe Final
  } deriving (Show, Eq)

data Final
  = Won
  | WonByDQ
  | Loss
  | LossByDQ
  | Tied
  deriving (Show, Eq)

data MatchInfo = MatchInfo
  { _miMatchId :: MatchId
  , _miMatchToken :: MatchToken
  } deriving (Show, Eq, Generic)


data RegisterRequest = RegisterRequest
  { _rreqName :: UserName
  } deriving (Show, Eq, Generic)

data RegisterResponse = RegisterResponse
  { _rrespCreds :: UserCreds
  } deriving (Show, Eq, Generic)

data UserCreds = UserCreds
  { _ucName :: UserName
  , _ucKey :: UserKey
  } deriving (Show, Eq, Generic)

data StartRequest = StartRequest
  { _sreqCreds :: UserCreds
  } deriving (Show, Eq, Generic)

data PlayRequest = PlayRequest
  { _preqCreds :: UserCreds
  , _preqLoc :: Loc
  } deriving (Show, Eq, Generic)

data StartResponse = StartResponse
  { _srespMatchInfo :: MatchInfo
  , _srespUsers :: Users
  , _srespState :: GameState
  } deriving (Show, Eq, Generic)

data PlayResponse = PlayResponse
  { _prespState :: GameState
  } deriving (Show, Eq, Generic)

data GameState = GameState
  { _gsBoard :: Board
  , _gsFinal :: Maybe Final
  } deriving (Show, Eq, Generic)

newtype UserKey = UserKey { getUserKey :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data UserConfig m = UserConfig
  { _userCfgUserName :: UserName
  , _userCfgMatchToken :: MatchToken
  , _userCfgSendLoc :: (Loc, Step -> m ()) -> m ()
  }

class Monad m => ServerEsque m where
  registerUser :: RegisterRequest -> m RegisterResponse
  playMove :: MatchId -> MatchToken -> PlayRequest -> m PlayResponse
  startMatch :: StartRequest -> m StartResponse
  randomMatch :: StartRequest -> m StartResponse

instance FromJSON RegisterRequest where
  parseJSON = dropPrefixP "_rreq"

instance ToJSON RegisterRequest where
  toJSON = dropPrefixJ "_rreq"

instance FromJSON RegisterResponse where
  parseJSON = dropPrefixP "_rresp"

instance ToJSON RegisterResponse where
  toJSON = dropPrefixJ "_rresp"

instance FromJSON UserCreds where
  parseJSON = dropPrefixP "_uc"

instance ToJSON UserCreds where
  toJSON = dropPrefixJ "_uc"

instance FromJSON StartRequest where
  parseJSON = dropPrefixP "_sreq"

instance ToJSON StartRequest where
  toJSON = dropPrefixJ "_sreq"

instance FromJSON PlayRequest where
  parseJSON = dropPrefixP "_preq"

instance ToJSON PlayRequest where
  toJSON = dropPrefixJ "_preq"

instance FromJSON StartResponse where
  parseJSON = dropPrefixP "_sresp"

instance ToJSON StartResponse where
  toJSON = dropPrefixJ "_sresp"

instance FromJSON PlayResponse where
  parseJSON = dropPrefixP "_presp"

instance ToJSON PlayResponse where
  toJSON = dropPrefixJ "_presp"

instance FromJSON GameState where
  parseJSON = dropPrefixP "_gs"

instance ToJSON GameState where
  toJSON = dropPrefixJ "_gs"

instance FromJSON Users where
  parseJSON = dropPrefixP "_u"

instance ToJSON Users where
  toJSON = dropPrefixJ "_u"

instance FromJSON MatchInfo where
  parseJSON = dropPrefixP "_mi"

instance ToJSON MatchInfo where
  toJSON = dropPrefixJ "_mi"

instance FromJSON Final where
  parseJSON (String "Won") = pure Won
  parseJSON (String "WonByDQ") = pure WonByDQ
  parseJSON (String "Loss") = pure Loss
  parseJSON (String "LossByDQ") = pure LossByDQ
  parseJSON (String "Tied") = pure Tied
  parseJSON _ = mzero

instance ToJSON Final where
  toJSON f = String $ case f of
    Won -> "Won"
    WonByDQ -> "WonByDQ"
    Loss -> "Loss"
    LossByDQ -> "LossByDQ"
    Tied -> "Tied"
