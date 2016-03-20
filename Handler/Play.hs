module Handler.Play where

import Import
import T3.Web
import T3.Match
import Handler.Instance ()

postPlayR :: MatchId -> MatchToken -> Handler Value
postPlayR = play
