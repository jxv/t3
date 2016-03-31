{-# OPTIONS -fno-warn-orphans #-}
module T3.Playback where

import Control.Monad (mzero)
import T3.Game
import T3.Match
import T3.Server ()
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HML

data Playback = Playback
  { pbMatchId :: MatchId
  , pbUsers :: Users
  , pbActions :: [Action]
  , pbResult :: Result
  } deriving (Show, Eq)

writePlayback :: FilePath -> Playback -> IO ()
writePlayback prefix pb = BL.writeFile path (encode pb)
  where
    (MatchId matchIdText) = pbMatchId pb
    path = prefix `mappend` (T.unpack $ matchIdText  `mappend` ".json")

instance ToJSON Playback where
  toJSON pb = object
    [ "matchId" .= pbMatchId pb
    , "users" .= pbUsers pb
    , "actions" .= pbActions pb
    , "result" .= pbResult pb
    ]

instance FromJSON Playback where
  parseJSON (Object o) = Playback
    <$> o .: "matchId"
    <*> o .: "users"
    <*> o .: "actions"
    <*> o .: "result"

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
