module T3.Match.Types where

import Prelude
import Data.Text (Text)
import T3.Game

type UserName = Text
type MatchId = Text
type MatchToken = Text

data MatchInfo = MatchInfo
  { miMatchId :: MatchId
  , miMatchToken :: MatchToken
  } deriving (Show, Eq)

data Step = Step
  { stepBoard :: Board
  } deriving (Show, Eq)

data Final
  = Won
  | WonByDQ
  | Loss
  | LossByDQ
  | Tied
  deriving (Show, Eq)
