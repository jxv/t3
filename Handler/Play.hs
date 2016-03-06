module Handler.Play where

import Import
import Respond
import T3.Session.Types
import T3.Web

postPlayR :: GameId -> GameToken -> Handler ()
postPlayR gameId gameToken = do
  userKey <- requireJsonBody 
  responder $ play gameId gameToken userKey
