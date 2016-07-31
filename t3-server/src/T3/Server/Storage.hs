{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fno-warn-orphans #-}
module T3.Server.Storage
  ( Storage(..)
  , Playback(..)
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HML
import qualified Data.Map as Map
import GHC.Generics
import Control.Monad (mzero)
import Data.Map (Map)
import Data.Aeson hiding (Result)

import qualified T3.Game as Game
import T3.Core (XO(..), Loc(..), Result(..), dropPrefixP, dropPrefixJ, yinYang, Action(..))
import T3.Game
import T3.Server.Match
import T3.Server (UserName, UserKey, MatchId, Users)
import T3.Server.Dispatch
import T3.Server.Lobby -- types

data Playback = Playback
  { _pbMatchId :: MatchId
  , _pbUsers :: Users
  , _pbActions :: [Action]
  , _pbResult :: Result
  } deriving (Show, Eq, Generic)

class Monad m => Storage m where
  storeUsers :: Map UserName UserKey -> m ()
  loadUsers :: m (Map UserName UserKey)
  loadMatchList :: m [MatchId]
  storePlayback :: Playback -> m ()
  loadPlayback :: MatchId -> m Playback

instance ToJSON Playback where
  toJSON pb = object
    [ "matchId" .= _pbMatchId pb
    , "users" .= _pbUsers pb
    , "actions" .= _pbActions pb
    , "result" .= _pbResult pb
    ]

instance FromJSON Playback where
  parseJSON = dropPrefixP "_pb"

instance ToJSON Result where
  toJSON Tie = object [ "tag" .= String "tie" ]
  toJSON Unfinished = object [ "tag" .= String "unfinished" ]
  toJSON (Winner xo) = object [ "tag" .= String "decision", "winner" .= xo, "loser" .= yinYang xo ]

instance FromJSON Result where
  parseJSON (Object o) = case HML.lookup "tag" o of
    Just (String "tie") -> pure Tie
    Just (String "unfinished") -> pure Unfinished
    Just (String "decision") -> case HML.lookup "winner" o of
      Just xo -> Winner <$> parseJSON xo
      _ -> mzero
    _ -> mzero
  parseJSON _ = mzero
