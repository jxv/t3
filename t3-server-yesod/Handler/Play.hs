module Handler.Play where

import Import
import T3.Web
import T3.Match
import Handler.Instance ()

postPlayR :: Text -> Text -> Handler Value
postPlayR a b = fmap toJSON (play (MatchId a) (MatchToken b))
