module Handler.Play where

import Import
import T3.Server
import T3.Match

postPlayR :: MatchId -> MatchToken -> Handler Value
postPlayR matchId matchToken = error "NYI"
--  userPlay <- requireJsonBody 
--  returnJson =<< play matchId matchToken userPlay
