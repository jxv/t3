{-# LANGUAGE DeriveGeneric #-}
module T3.Server.Match
  ( Match(..)
  , UserName(..)
  , MatchId(..)
  , MatchToken(..)
  , Users(..)
  , MatchInfo(..)
  , Step(..)
  , Final(..)
  , MatchData(..)
  , Seconds(..)
  , Callback
  , StartCallback
  , UserInit
  ) where

import GHC.Generics
import Control.Monad (mzero)
import Data.Char (toLower)
import Data.Aeson hiding (Result)
import Data.Aeson.Types hiding (Result)
import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result, Action(..), dropPrefixP, dropPrefixJ)
import T3.Game -- types

type Callback m = Step -> m ()

type StartCallback m = MatchInfo -> Users -> Step -> m ()

type UserInit m = (Callback m, m (Loc, Callback m))

newtype Seconds = Seconds Int
  deriving (Num, Show, Eq, Ord, Enum)

data MatchData m = MatchData
  { _matchReq :: XO -> m (Loc, Callback m)
  , _matchRespX :: Callback m
  , _matchRespO :: Callback m
  , _matchLog :: [Action] -> Board -> Result -> m ()
  , _matchBoard :: Board
  , _matchActions :: [Action]
  , _matchTimeoutLimit :: Maybe Seconds
  }

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

class Monad m => Match m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Result -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()
