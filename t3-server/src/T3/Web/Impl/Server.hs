{-# OPTION_GHC -fno-warn-orphans #-}
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
import Safe (atMay)
import Network.HTTP.Types

import T3.WebLang hiding (Web(..))
import T3.Server
import T3.Server.Dispatch
import T3.Server.Lobby
import T3.DB
import T3.Match
import T3.Random
import T3.Game.Core

class MonadIO m => ServerEsque m where
  httpRequestEntity :: m BL.ByteString
  server :: m (Server IO)
  alreadyInLobby :: m a
  registerUser :: Server IO -> UserName -> UserKey -> m (M.Map UserName UserKey)
  playMove :: MatchId -> MatchToken -> PlayRequest -> m (Maybe PlayResponse)

badRequest, badFormat, unauthorized :: Response
badRequest = Response status400 [] Nothing
badFormat = Response status400 [] (Just "FORMATTING ERROR")
unauthorized = Response status401 [] Nothing

-- /api/play/<match-id>/<match-token>
play :: ServerEsque m => Request -> m Response
play req = do
  let m = (,,)
        <$> (MatchId <$> atMay (_reqPath req) 2)
        <*> (MatchToken <$> atMay (_reqPath req) 3)
        <*> (decode $ _reqBody req)
  case m of
    Nothing -> return badFormat
    Just (matchId, matchToken, playReq) ->
      fromPlayResponse <$> play' matchId matchToken playReq
  where
    fromPlayResponse :: Maybe PlayResponse -> Response
    fromPlayResponse (Just presp) = Response status200 [] (Just $ encode presp)
    fromPlayResponse Nothing = badRequest

play' :: ServerEsque m => MatchId -> MatchToken -> PlayRequest -> m (Maybe PlayResponse)
play' = playMove

playMove' :: Server IO -> MatchId -> MatchToken -> PlayRequest -> IO (Maybe PlayResponse)
playMove' srv matchId matchToken playReq = do
  mUserCfg <- userConfig' srv matchId matchToken playReq
  case mUserCfg of
    Nothing -> return Nothing
    Just userCfg -> do
      resp <- liftIO newEmptyMVar
      liftIO $ (_userCfgSendLoc userCfg) (_preqLoc playReq, putMVar resp . PlayResponse . toGameState)
      liftIO $ (either id id) <$> race (Just <$> takeMVar resp) (delay (Seconds 60) >> return Nothing)

userConfig' :: Server IO -> MatchId -> MatchToken -> PlayRequest -> IO (Maybe (UserConfig IO))
userConfig' srv matchId matchToken playReq = liftIO . atomically $ do
  let creds = _preqCreds playReq
  authenicated <- authenticate srv creds
  if not authenicated
    then return Nothing
    else do
      mMatchCfg <- M.lookup matchId <$> readTVar (_srvMatches srv)
      return $ authorize (_ucName creds) matchToken =<< mMatchCfg

-- /api/start
start :: ServerEsque m => Request -> m Response
start req = do
  let m = decode $ _reqBody req
  case m of
    Nothing -> return badFormat
    Just startReq -> fromStartResponse <$> start' startReq
  where
    fromStartResponse :: (Maybe StartResponse) -> Response
    fromStartResponse (Just sresp) = Response status200 [] (Just $ encode sresp)
    fromStartResponse Nothing = unauthorized

start' :: ServerEsque m => StartRequest -> m (Maybe StartResponse)
start' startReq = do
  srv <- server
  resp <- liftIO newEmptyMVar
  authenticated <- liftIO . atomically $ authenticate srv (_sreqCreds startReq)
  if not authenticated
    then return Nothing
    else fmap Just $ do
      added <- liftIO $ addUserToLobby
        (_srvLobby srv)
        (_ucName $ _sreqCreds startReq)
        (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
      if added
        then do
          sresp <- liftIO $ takeMVar resp
          return sresp
        else alreadyInLobby

-- /api/random
randomHandler :: ServerEsque m => Request -> m Response
randomHandler req = do
  let m = decode $ _reqBody req
  case m of
    Nothing -> return badFormat
    Just startReq -> fromStartResponse <$> randomHandler' startReq
  where
    fromStartResponse :: (Maybe StartResponse) -> Response
    fromStartResponse (Just sresp) = Response status200 [] (Just $ encode sresp)
    fromStartResponse Nothing = unauthorized

randomHandler' :: ServerEsque m => StartRequest -> m (Maybe StartResponse)
randomHandler' startReq = do
  srv <- server
  authenticated <- liftIO . atomically $ authenticate srv (_sreqCreds startReq)
  if not authenticated
    then return Nothing
    else fmap Just $ do
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

-- /api/register
register :: (ServerEsque m, DB m) => Request -> m Response
register req = do
  let m = decode (_reqBody req)
  case m of
    Nothing -> return badFormat
    Just rreq -> do
      let (UserName un) = _rreqName rreq
      if T.null un
        then return badRequest
        else fromRegisterResponse <$> register' rreq
  where
    fromRegisterResponse :: RegisterResponse -> Response
    fromRegisterResponse rresp = Response status200 [] (Just $ encode rresp)

register' :: (ServerEsque m, DB m) => RegisterRequest -> m RegisterResponse
register' rreq = do
  let name@(UserName un) = _rreqName rreq
  srv <- server
  userKey <- liftIO genUserKey
  users <- registerUser srv name userKey
  storeUsers users
  return $ RegisterResponse (UserCreds name userKey)

registerUser' :: Server IO -> UserName -> UserKey -> IO (Maybe (M.Map UserName UserKey))
registerUser' srv name userKey = liftIO . atomically $ do
  users <- readTVar (_srvUsers srv)
  let users' = M.insert name userKey users
  if M.member name users
    then return Nothing
    else writeTVar (_srvUsers srv) users' >> return (Just users')

