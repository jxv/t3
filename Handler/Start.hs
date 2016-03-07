module Handler.Start where

import Import
import T3.Web
import T3.Service
import T3.Web.Instance ()

postStartR :: Handler Value
postStartR = error "NYI"
--  userStart <- requireJsonBody 
--  returnJson =<< start userStart
