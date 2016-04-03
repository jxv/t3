{-# LANGUAGE DeriveGeneric #-}

module T3.Match.Types
  ( UserName(..)
  , MatchId(..)
  , MatchToken(..)
  , Users(..)
  , MatchInfo(..)
  , Step(..)
  , Final(..)
  ) where

import GHC.Generics
import Control.Monad (mzero)
import Data.Char (toLower)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import T3.Game
import T3.Game.Core

newtype UserName = UserName Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype MatchId = MatchId Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype MatchToken = MatchToken Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data Users = Users
  { _uX :: UserName
  , _uO :: UserName
  } deriving (Show, Eq, Generic)

instance FromJSON Users where
  parseJSON = dropPrefixP "_u"

instance ToJSON Users where
  toJSON = dropPrefixJ "_u"

data MatchInfo = MatchInfo
  { _miMatchId :: MatchId
  , _miMatchToken :: MatchToken
  } deriving (Show, Eq, Generic)

instance FromJSON MatchInfo where
  parseJSON = dropPrefixP "_mi"

instance ToJSON MatchInfo where
  toJSON = dropPrefixJ "_mi"

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
