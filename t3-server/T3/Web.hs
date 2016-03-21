module T3.Web where

import Prelude
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Aeson
import Control.Concurrent
import Control.Concurrent.STM
import T3.Server
import T3.Server.Lobby
import T3.Match
import Control.Monad.Trans (MonadIO, liftIO)

class (MonadIO m) => HttpHandler m where
  httpRequestEntity :: m BL.ByteString
  server :: m Server

play :: HttpHandler m => MatchId -> MatchToken -> m Value
play matchId matchToken = do
  srv <- server
  entity <- httpRequestEntity
  let (Just playReq) = decode entity
  mUserCfg <- liftIO . atomically $ do
    let creds = preqUserCreds playReq
    authenicated <- authenticate srv creds
    if not authenicated
      then return Nothing
      else do
        mMatchCfg <- M.lookup matchId <$> readTVar (srvMatches srv)
        return $ authorize (ucUserName creds) matchToken =<< mMatchCfg
  case mUserCfg of
    Nothing -> return (toJSON ([] :: [()]))
    Just userCfg -> do
      resp <- liftIO newEmptyMVar
      liftIO $ (userCfgSendLoc userCfg) (preqLoc playReq, putMVar resp . PlayResponse . toGameState)
      presp <- liftIO $ readMVar resp
      return $ toJSON presp

start :: HttpHandler m => m Value
start = do
  srv <- server
  entity <- httpRequestEntity
  let (Just startReq) = decode entity
  resp <- liftIO newEmptyMVar
  authenticated <- liftIO . atomically $ authenticate srv (sreqUserCreds startReq)
  if not authenticated
    then return (toJSON (Nothing :: Maybe ()))
    else do
      added <- liftIO $ addUserToLobby
        (srvLobby srv)
        (ucUserName $ sreqUserCreds startReq)
        (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
      if added
        then do
          sresp <- liftIO $ readMVar resp
          return $ toJSON sresp
        else return $ toJSON (Nothing :: Maybe ())

data RegisterResult
  = NameExists
  | NoName
  | Good UserName UserKey

register :: HttpHandler m => UserName -> m RegisterResult
register name = do
  srv <- server
  if T.null name
    then return NoName
    else do
      userKey <- liftIO genUserKey
      exists <- liftIO . atomically $ do
        users <- readTVar (srvUsers srv)
        if M.member name users
          then return True
          else writeTVar (srvUsers srv) (M.insert name userKey users) >> return False
      return $ if exists then NameExists else Good name userKey
