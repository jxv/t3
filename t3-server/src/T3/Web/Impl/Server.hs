module T3.Web.Impl.Server where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Conc.ClassTmp (MonadConc(..))
import Control.Concurrent.STM (modifyTVar, readTVar, writeTVar)
import Control.Monad (mzero, forever)
import Data.Aeson
import Data.IORef
import Data.Maybe
import Control.Monad.Trans (MonadIO, liftIO)

import T3.WebLang hiding (Web(..))
import T3.Server
import T3.Server.Dispatch
import T3.Server.Lobby
import T3.DB
import T3.Match
import T3.Random
import T3.Game.Core

class MonadIO m => HttpHandler m where
  httpRequestEntity :: m BL.ByteString
  server :: m (Server IO)
  unauthorized :: m a
  badRequest :: m a
  badFormat :: m a
  alreadyInLobby :: m a
  --
  httpJSONEntity :: FromJSON a => m (Maybe a)
  httpJSONEntity = fmap decode httpRequestEntity

play :: HttpHandler m => MatchId -> MatchToken -> Maybe PlayRequest -> m PlayResponse
play _ _ Nothing = badFormat
play matchId matchToken (Just playReq) = play' matchId matchToken playReq

play' :: HttpHandler m => MatchId -> MatchToken -> PlayRequest -> m PlayResponse
play' matchId matchToken playReq = do
  srv <- server
  mUserCfg <- liftIO . atomically $ do
    let creds = _preqCreds playReq
    authenicated <- authenticate srv creds
    if not authenicated
      then return Nothing
      else do
        mMatchCfg <- M.lookup matchId <$> readTVar (_srvMatches srv)
        return $ authorize (_ucName creds) matchToken =<< mMatchCfg
  case mUserCfg of
    Nothing -> unauthorized
    Just userCfg -> do
      resp <- liftIO newEmptyMVar
      liftIO $ (_userCfgSendLoc userCfg) (_preqLoc playReq, putMVar resp . PlayResponse . toGameState)
      mPresp <- liftIO $ (either id id) <$> race (Just <$> takeMVar resp) (delay (Seconds 60) >> return Nothing)
      fromMaybe badRequest (return <$> mPresp)

start :: HttpHandler m => Maybe StartRequest -> m StartResponse
start Nothing = badFormat
start (Just startReq) = start' startReq

start' :: HttpHandler m => StartRequest -> m StartResponse
start' startReq = do
  srv <- server
  resp <- liftIO newEmptyMVar
  authenticated <- liftIO . atomically $ authenticate srv (_sreqCreds startReq)
  if not authenticated
    then unauthorized
    else do
      added <- liftIO $ addUserToLobby
        (_srvLobby srv)
        (_ucName $ _sreqCreds startReq)
        (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
      if added
        then do
          sresp <- liftIO $ takeMVar resp
          return sresp
        else alreadyInLobby

randomHandler :: HttpHandler m => Maybe StartRequest -> m StartResponse
randomHandler mStartReq = case mStartReq of
  Nothing -> badFormat
  Just startReq -> randomHandler' startReq

randomHandler' :: HttpHandler m => StartRequest -> m StartResponse
randomHandler' startReq = do
  srv <- server
  authenticated <- liftIO . atomically $ authenticate srv (_sreqCreds startReq)
  if not authenticated
    then unauthorized
    else do
      matchId <- liftIO genMatchId
      xGT <- liftIO genMatchToken
      oGT <- liftIO genMatchToken
      randomStep <- liftIO newEmptyMVar
      let randomCB = putMVar randomStep
      randomSendLocRef <- liftIO $ newIORef (const $ return ())
      randomThid <- liftIO . fork . forever $ do
        step <- takeMVar randomStep
        mLoc <- randomLoc (_stepBoard step)
        case mLoc of
          Nothing -> return ()
          Just loc -> do
            sendLoc <- readIORef randomSendLocRef
            sendLoc (loc, randomCB)
      let xUN = _ucName (_sreqCreds startReq)
      let oUN = UserName "random"
      let removeSelf = do
            killThread randomThid
            atomically $ modifyTVar (_srvMatches srv) (M.delete matchId)
      let users = Users { _uX = xUN, _uO = oUN }
      let xMatchInfo = MatchInfo matchId xGT
      sessCfg <- liftIO $ forkMatch
        (_srvTimeoutLimit srv)
        (xUN, xGT, const $ return ())
        (oUN, oGT, randomCB)
        (\_ _ _ -> return ())
        removeSelf
      liftIO $ writeIORef randomSendLocRef (_userCfgSendLoc $ _matchCfgO sessCfg)
      liftIO . atomically $ modifyTVar (_srvMatches srv) (M.insert matchId sessCfg)
      return $ StartResponse xMatchInfo Users{ _uX = xUN, _uO = oUN } (GameState emptyBoard Nothing)

register :: (HttpHandler m, DB m) => Maybe RegisterRequest -> m (Maybe RegisterResponse)
register Nothing = badFormat
register (Just rreq) = register' rreq

register' :: (HttpHandler m, DB m) => RegisterRequest -> m (Maybe RegisterResponse)
register' rreq = do
  let name@(UserName un) = _rreqName rreq
  srv <- server
  if T.null un
    then badRequest
    else do
      userKey <- liftIO genUserKey
      mUsers <- liftIO . atomically $ do
        users <- readTVar (_srvUsers srv)
        let users' = M.insert name userKey users
        if M.member name users
          then return Nothing
          else writeTVar (_srvUsers srv) users' >> return (Just users')
      case mUsers of
        Nothing -> badRequest
        Just users -> do
          storeUsers users
          return . Just $ RegisterResponse (UserCreds name userKey)
