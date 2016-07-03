{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fno-warn-orphans #-}
module T3.Storage
  ( Storage(..)
  , Action(..)
  , Playback(..)
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HML
import GHC.Generics
import Control.Monad (mzero)
import Data.Aeson hiding (Result)

import qualified T3.Game as Game
import T3.Core (XO(..), Loc(..), Result(..), dropPrefixP, dropPrefixJ, yinYang)
import T3.Game hiding (Action(..))
import T3.Match hiding (Action(..))

import qualified Data.Map as M

import T3.Server -- types
import T3.Dispatch hiding (Action) -- types
import T3.Lobby -- types
import T3.Match -- types

data Action = Action
  { _actXO :: XO
  , _actLoc :: Loc
  } deriving (Show, Eq, Generic)

data Playback = Playback
  { _pbMatchId :: MatchId
  , _pbUsers :: Users
  , _pbActions :: [Action]
  , _pbResult :: Result
  } deriving (Show, Eq, Generic)

instance ToJSON Action where
  toJSON = dropPrefixJ "_act"

instance FromJSON Action where
  parseJSON = dropPrefixP "_act"

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

class Monad m => Storage m where
  storeUsers :: M.Map UserName UserKey -> m ()
  loadUsers :: m (M.Map UserName UserKey)
  loadMatchList :: m [MatchId]
  storePlayback :: Playback -> m ()
  loadPlayback :: MatchId -> m Playback
