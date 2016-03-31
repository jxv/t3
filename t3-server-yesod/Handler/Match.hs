module Handler.Match where

import Import
import T3.DB
import T3.Match
import Handler.Instance ()

getMatchR :: Text -> Handler Value
getMatchR matchId = do
  mPB <- loadPlayback (MatchId matchId)
  case mPB of
    Nothing -> notFound
    Just pb -> returnJson pb
