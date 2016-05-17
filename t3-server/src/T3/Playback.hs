{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fno-warn-orphans #-}
module T3.Playback where

import GHC.Generics
import Control.Monad (mzero)
import T3.Game hiding (Action(..))
import T3.Match.Types hiding (Action(..))
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HML
import qualified T3.Game as Game

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

writePlayback :: FilePath -> Playback -> IO ()
writePlayback prefix pb = BL.writeFile path (encode pb)
  where
    (MatchId matchIdText) = _pbMatchId pb
    path = prefix `mappend` (T.unpack $ matchIdText  `mappend` ".json")

instance ToJSON Action where
  toJSON = Game.dropPrefixJ "_act"

instance FromJSON Action where
  parseJSON = Game.dropPrefixP "_act"

instance ToJSON Playback where
  toJSON pb = object
    [ "matchId" .= _pbMatchId pb
    , "users" .= _pbUsers pb
    , "actions" .= _pbActions pb
    , "result" .= _pbResult pb
    ]

instance FromJSON Playback where
  parseJSON = Game.dropPrefixP "_pb"

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
