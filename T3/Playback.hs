{-# OPTIONS -fno-warn-orphans #-}
module T3.Playback where

import Prelude
import T3.Game
import T3.Match
import T3.Server ()
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

writePlayback :: FilePath -> MatchId -> Users -> [Action] -> Result -> IO ()
writePlayback prefix matchId users actions res = BL.writeFile path (encode val)
  where
    path = prefix `mappend` (T.unpack $ matchId `mappend` ".json")
    val = object
      [ "matchId" .= matchId
      , "users" .= users
      , "actions" .= actions
      , "result" .= res
      ]

instance ToJSON Result where
  toJSON Tie = object [ "tag" .= String "tie" ]
  toJSON Unfinished = object [ "tag" .= String "unfinished" ]
  toJSON (Winner xo) = object [ "tag" .= String "divide", "winner" .= xo, "loser" .= yinYang xo ]
