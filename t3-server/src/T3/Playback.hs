{-# OPTIONS -fno-warn-orphans #-}
module T3.Playback where

import Prelude
import T3.Game
import T3.Match
import T3.Server ()
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

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

instance ToJSON Result where
  toJSON Tie = object [ "tag" .= String "tie" ]
  toJSON Unfinished = object [ "tag" .= String "unfinished" ]
  toJSON (Winner xo) = object [ "tag" .= String "decision", "winner" .= xo, "loser" .= yinYang xo ]
