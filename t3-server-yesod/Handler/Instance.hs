{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Instance where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as Wai
import Control.Concurrent
import Import
import T3.Server
import T3.Web
import T3.DB
import Data.Aeson (encode, decode)

instance HttpHandler Handler where
  httpRequestEntity = do
    req <- waiRequest
    BL.fromStrict <$> liftIO (Wai.requestBody req)
  unauthorized = notAuthenticated
  badRequest = invalidArgs []
  badFormat = invalidArgs ["Bad JSON format"]
  alreadyInLobby = permissionDenied "Already in lobby"
  server = fmap appServer getYesod

instance DB IO where
  -- TODO: make transactional
  storeUsers users = void . forkIO $ do
    let users' = M.mapKeys (\(UserName un) -> un) users
    BL.writeFile "db/user.json" (encode users')
  loadUsers = handle (\(_ :: SomeException) -> return M.empty) $ do
    rawUsers <- BL.readFile "db/user.json"
    return $ maybe M.empty (M.mapKeys UserName) (decode rawUsers)

instance DB Handler where
  storeUsers = liftIO . storeUsers
  loadUsers = liftIO loadUsers
