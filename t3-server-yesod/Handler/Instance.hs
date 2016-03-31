{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Instance where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as Wai
import qualified Data.Text as T
import Control.Concurrent
import Import
import T3.Server
import T3.Match
import T3.Web
import T3.DB
import T3.Playback
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

instance DB Handler where
  storeUsers = liftIO . storeUsers
  loadUsers = liftIO loadUsers
  loadMatchList = liftIO loadMatchList
  storePlayback = liftIO . storePlayback
  loadPlayback = liftIO . loadPlayback

instance DB IO where
  -- TODO: make transactional   D:
  storeUsers users = treadOn () $ void . forkIO $ do
    let users' = M.mapKeys (\(UserName un) -> un) users
    BL.writeFile "db/user.json" (encode users')
  loadUsers = treadOn M.empty $ do
    raw <- BL.readFile "db/user.json"
    case decode raw of
      Nothing -> return M.empty
      Just users -> return $ M.mapKeys UserName users
  loadMatchList = treadOn [] $ do
    raw <- BL.readFile "db/match.json"
    case decode raw of
      Nothing -> return []
      Just list -> return list
  storePlayback pb = treadOn () $ void . forkIO $ do
    writePlayback "db/match/" pb
  loadPlayback (MatchId matchId) = treadOn Nothing $ do
    raw <- BL.readFile $ T.unpack $ "db/match/" `mappend` matchId `mappend` ".json"
    return (decode raw)

treadOn :: a -> IO a -> IO a
treadOn a = handle (\(_ :: SomeException) -> return a)
