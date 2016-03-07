module Handler.Play where

import Import
import T3.Web
import T3.Web.Instance ()
import T3.Service

postPlayR :: GameId -> GameToken -> Handler Value
postPlayR gameId gameToken = error "NYI"
--  userPlay <- requireJsonBody 
--  returnJson =<< play gameId gameToken userPlay
