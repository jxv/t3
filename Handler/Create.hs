module Handler.Create where

import Import
import T3.Server

getCreateR :: Handler Html
getCreateR = error "Not yet implemented: getCreateR"

postCreateR :: Handler Value
postCreateR = do
  userId <- liftIO genUserId
  userKey <- liftIO genUserKey
  let creds = object [ "userId" .= userId, "userKey" .= userKey ]
  liftIO $ print creds
  returnJson creds
