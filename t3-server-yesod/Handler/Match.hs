module Handler.Match where

import Import

getMatchR :: String -> Handler ()
getMatchR matchId = sendFile "application/json" ("static/playback/" `mappend` matchId `mappend` ".json")
