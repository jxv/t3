{-# LANGUAGE DeriveGeneric #-}
module T3.Match.Types
  ( MatchId(..)
  , UserName(..)
  , Step(..)
  , Users(..)
  , Final(..)
  ) where

import GHC.Generics
import Control.Monad (mzero)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result, Action(..), dropPrefixP, dropPrefixJ)

newtype UserName = UserName { getUserName :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype MatchId = MatchId { getMatchId :: Text }
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

instance FromJSON Users where
  parseJSON = dropPrefixP "_u"

instance ToJSON Users where
  toJSON = dropPrefixJ "_u"

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
