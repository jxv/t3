module T3.Match.Types where

import Prelude
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import T3.Game

newtype UserName = UserName Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype MatchId = MatchId Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype MatchToken = MatchToken Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data Users = Users
  { uX :: UserName
  , uO :: UserName
  } deriving (Show, Eq)

data MatchInfo = MatchInfo
  { miMatchId :: MatchId
  , miMatchToken :: MatchToken
  } deriving (Show, Eq)

data Step = Step
  { stepBoard :: Board
  , stepFinal :: Maybe Final
  } deriving (Show, Eq)

data Final
  = Won
  | WonByDQ
  | Loss
  | LossByDQ
  | Tied
  deriving (Show, Eq)
