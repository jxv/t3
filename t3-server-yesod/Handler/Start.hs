module Handler.Start where

import Import
import T3.Web
import Handler.Instance ()

postStartR :: Handler Value
postStartR = fmap toJSON (start =<< httpJSONEntity)
