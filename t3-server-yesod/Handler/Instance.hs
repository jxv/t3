{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Instance where

import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as Wai
import Import
import T3.Web

instance HttpHandler Handler where
  httpRequestEntity = do
    req <- waiRequest
    BL.fromStrict <$> liftIO (Wai.requestBody req)
  unauthorized = notAuthenticated
  badFormat = invalidArgs ["Bad JSON format"]
  alreadyInLobby = permissionDenied "Already in lobby"
  server = fmap appServer getYesod
