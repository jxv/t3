module Handler.Random where

import Import
import Handler.Instance ()
import T3.Web

postRandomR :: Handler Value
postRandomR = fmap toJSON $ randomHandler =<< httpJSONEntity
