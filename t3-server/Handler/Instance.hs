{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Instance where

import Import
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as Wai
import T3.Web

instance HttpHandler Handler where
  httpRequestEntity = do
    req <- waiRequest
    BL.fromStrict <$> liftIO (Wai.requestBody req)
  server = fmap appServer getYesod
