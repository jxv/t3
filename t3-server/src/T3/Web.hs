module T3.Web where

import Prelude
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (mzero)
import Data.Aeson
import T3.Server
import T3.Server.Lobby
import T3.Match
import T3.Game.Core
import Control.Monad.Trans (MonadIO, liftIO)

class (MonadIO m) => HttpHandler m where
  httpRequestEntity :: m BL.ByteString
  server :: m Server
  httpJSONEntity :: FromJSON a => m (Maybe a)
  httpJSONEntity = fmap decode httpRequestEntity

play :: HttpHandler m => MatchId -> MatchToken -> Maybe PlayRequest -> m (Maybe PlayResponse)
play matchId matchToken mPlayRequest = do
  srv <- server
  case mPlayRequest of
    Nothing -> return Nothing
    Just playReq -> do
      mUserCfg <- liftIO . atomically $ do
        let creds = preqCreds playReq
        authenicated <- authenticate srv creds
        if not authenicated
          then return Nothing
          else do
            mMatchCfg <- M.lookup matchId <$> readTVar (srvMatches srv)
            return $ authorize (ucName creds) matchToken =<< mMatchCfg
      case mUserCfg of
        Nothing -> return Nothing
        Just userCfg -> do
          resp <- liftIO newEmptyMVar
          liftIO $ (userCfgSendLoc userCfg) (preqLoc playReq, putMVar resp . PlayResponse . toGameState)
          presp <- liftIO $ takeMVar resp
          return $ Just presp

start :: HttpHandler m => Maybe StartRequest -> m (Maybe StartResponse)
start mStartReq = do
  srv <- server
  case mStartReq of
    Nothing -> return Nothing
    Just startReq -> do
      resp <- liftIO newEmptyMVar
      authenticated <- liftIO . atomically $ authenticate srv (sreqCreds startReq)
      if not authenticated
        then return $ Nothing
        else do
          added <- liftIO $ addUserToLobby
            (srvLobby srv)
            (ucName $ sreqCreds startReq)
            (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
          if added
            then do
              sresp <- liftIO $ takeMVar resp
              return $ Just sresp
            else return $ Nothing 

register :: HttpHandler m => Maybe RegisterRequest -> m (Maybe (Either RegisterError RegisterResponse))
register Nothing = return Nothing
register (Just rreq) = fmap Just $ do
  let name@(UserName un) = rreqName rreq
  srv <- server
  if T.null un
    then return $ Left NoName
    else do
      userKey <- liftIO genUserKey
      exists <- liftIO . atomically $ do
        users <- readTVar (srvUsers srv)
        if M.member name users
          then return True
          else writeTVar (srvUsers srv) (M.insert name userKey users) >> return False
      return $ if exists then Left NameExists else Right $ RegisterResponse (UserCreds name userKey)
