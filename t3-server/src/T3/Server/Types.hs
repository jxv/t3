{-# LANGUAGE DeriveGeneric #-}
module T3.Server.Types
  ( RegisterRequest(..)
  , RegisterResponse(..)
  , StartRequest(..)
  , StartResponse(..)
  , PlayRequest(..)
  , PlayResponse(..)
  , MatchId(..)
  , MatchToken(..)
  , UserKey(..)
  , UserCreds(..)
  , GameState(..)
  , UserConfig(..)
  ) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

import T3.Match.Types (UserName, MatchInfo, MatchId, MatchToken, Final, Users, Callback)
import T3.Game.Core (dropPrefixJ, dropPrefixP, Loc, Board)

data RegisterRequest = RegisterRequest
  { _rreqName :: UserName
  } deriving (Show, Eq, Generic)

instance FromJSON RegisterRequest where
  parseJSON = dropPrefixP "_rreq"

instance ToJSON RegisterRequest where
  toJSON = dropPrefixJ "_rreq"

data RegisterResponse = RegisterResponse
  { _rrespCreds :: UserCreds
  } deriving (Show, Eq, Generic)

instance FromJSON RegisterResponse where
  parseJSON = dropPrefixP "_rresp"

instance ToJSON RegisterResponse where
  toJSON = dropPrefixJ "_rresp"

data UserCreds = UserCreds
  { _ucName :: UserName
  , _ucKey :: UserKey
  } deriving (Show, Eq, Generic)

instance FromJSON UserCreds where
  parseJSON = dropPrefixP "_uc"

instance ToJSON UserCreds where
  toJSON = dropPrefixJ "_uc"

data StartRequest = StartRequest
  { _sreqCreds :: UserCreds
  } deriving (Show, Eq, Generic)

instance FromJSON StartRequest where
  parseJSON = dropPrefixP "_sreq"

instance ToJSON StartRequest where
  toJSON = dropPrefixJ "_sreq"

data PlayRequest = PlayRequest
  { _preqCreds :: UserCreds
  , _preqLoc :: Loc
  } deriving (Show, Eq, Generic)

instance FromJSON PlayRequest where
  parseJSON = dropPrefixP "_preq"

instance ToJSON PlayRequest where
  toJSON = dropPrefixJ "_preq"

data StartResponse = StartResponse
  { _srespMatchInfo :: MatchInfo
  , _srespUsers :: Users
  , _srespState :: GameState
  } deriving (Show, Eq, Generic)

instance FromJSON StartResponse where
  parseJSON = dropPrefixP "_sresp"

instance ToJSON StartResponse where
  toJSON = dropPrefixJ "_sresp"

data PlayResponse = PlayResponse
  { _prespState :: GameState
  } deriving (Show, Eq, Generic)

instance FromJSON PlayResponse where
  parseJSON = dropPrefixP "_presp"

instance ToJSON PlayResponse where
  toJSON = dropPrefixJ "_presp"

data GameState = GameState
  { _gsBoard :: Board
  , _gsFinal :: Maybe Final
  } deriving (Show, Eq, Generic)

instance FromJSON GameState where
  parseJSON = dropPrefixP "_gs"

instance ToJSON GameState where
  toJSON = dropPrefixJ "_gs"

newtype UserKey = UserKey { getUserKey :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data UserConfig m = UserConfig
  { _userCfgUserName :: UserName
  , _userCfgMatchToken :: MatchToken
  , _userCfgSendLoc :: (Loc, Callback m) -> m ()
  }
