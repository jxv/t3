module T3.Match.Types where

import Prelude
import Data.Text (Text)

type UserId = Text
type MatchId = Text
type MatchToken = Text

data Final
  = Won
  | WonByDQ
  | Loss
  | LossByDQ
  | Tied
  deriving (Show, Eq)
