module Handler.Register where

import Import
import Handler.Instance ()
import T3.Web

postRegisterR :: Handler Value
postRegisterR = fmap toJSON $ register =<< httpJSONEntity
