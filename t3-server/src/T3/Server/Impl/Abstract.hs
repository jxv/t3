module T3.Server.Impl.Abstract where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Conc.ClassTmp
import Control.Monad.Random
import Control.Concurrent.STM (modifyTVar, readTVar, writeTVar, TVar, STM)
import Control.Monad (mzero, forever)
import Data.Aeson hiding (Result(..))
import Data.IORef
import Data.Maybe
import Control.Monad.Trans (MonadIO, liftIO)
import Safe (atMay)
import Network.HTTP.Types

import T3.Server.Types
import T3.Server.Dispatch.Impl.MonadConc
import T3.Server.Dispatch.Types
import T3.Server.Lobby.Impl.MonadConc
import T3.Server.Lobby.Types
import T3.DB
import T3.Match
import T3.Match.Types
import T3.Random
import T3.Game.Core
import T3.Util

data Server m = Server
  { _srvLobby :: ListLobby m
  , _srvMatches :: TVar (M.Map MatchId (MatchConfig m))
  , _srvUsers :: TVar (M.Map UserName UserKey)
  , _srvDie :: m ()
  , _srvLogger :: GameLogger m
  , _srvTimeoutLimit :: Maybe Seconds
  }

type GameLogger m = MatchId -> Users -> [Action] -> Board -> Result -> m ()

genMatchToken :: MonadRandom m => m MatchToken
genMatchToken = MatchToken <$> genBase64 16

genMatchId :: MonadRandom m => m MatchId
genMatchId = MatchId <$> genBase64 16

genUserName :: MonadRandom m => m UserName
genUserName = UserName <$> genBase64 32

genUserKey :: MonadRandom m => m UserKey
genUserKey = UserKey <$> genBase64 32

authenticate :: MonadConc m => Server m -> UserCreds -> STM Bool
authenticate srv uc = do
  users <- readTVar (_srvUsers srv)
  return $ M.lookup (_ucName uc) users == Just (_ucKey uc)

authorize :: UserName -> MatchToken -> MatchConfig m -> Maybe (UserConfig m)
authorize un mt mc = (userCfgMay $ _matchCfgX mc) <|> (userCfgMay $ _matchCfgO mc)
  where
    userCfgMay cfg =
      if _userCfgUserName cfg == un && _userCfgMatchToken cfg == mt
        then Just cfg
        else Nothing

toGameState :: Step -> GameState
toGameState s = GameState (_stepBoard s) (_stepFinal s)

-- ServerEsque.playMove :: MatchId -> MatchToken -> PlayRequest -> m (Maybe PlayResponse)
playMove :: Server IO -> MatchId -> MatchToken -> PlayRequest -> IO (Maybe PlayResponse)
playMove srv matchId matchToken playReq = do
  mUserCfg <- userConfig srv matchId matchToken playReq
  case mUserCfg of
    Nothing -> return Nothing
    Just userCfg -> do
      resp <- liftIO newEmptyMVar
      liftIO $ (_userCfgSendLoc userCfg) (_preqLoc playReq, putMVar resp . PlayResponse . toGameState)
      liftIO $ (either id id) <$> race (Just <$> takeMVar resp) (delay (Seconds 60) >> return Nothing)

userConfig :: Server IO -> MatchId -> MatchToken -> PlayRequest -> IO (Maybe (UserConfig IO))
userConfig srv matchId matchToken playReq = liftIO . atomically $ do
  let creds = _preqCreds playReq
  authenicated <- authenticate srv creds
  if not authenicated
    then return Nothing
    else do
      mMatchCfg <- M.lookup matchId <$> readTVar (_srvMatches srv)
      return $ authorize (_ucName creds) matchToken =<< mMatchCfg

-- ServerEsque.startMatch :: StartRequest -> m (Maybe StartResponse)
startMatch :: Server IO -> StartRequest -> IO (Maybe StartResponse)
startMatch srv startReq = do
  resp <- newEmptyMVar
  authenticated <- atomically $ authenticate srv (_sreqCreds startReq)
  if not authenticated
    then return Nothing
    else do
      added <- addUserToLobby
        (_srvLobby srv)
        (_ucName $ _sreqCreds startReq)
        (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
      if added
        then do
          sresp <- takeMVar resp
          return $ Just sresp
        else return Nothing

-- ServerEsque.randomMatch :: StartRequest -> m (Maybe StartResponse)
randomMatch :: Server IO -> StartRequest -> IO (Maybe StartResponse)
randomMatch srv startReq = do
  authenticated <- atomically $ authenticate srv (_sreqCreds startReq)
  if not authenticated
    then return Nothing
    else fmap Just $ do
      matchId <- genMatchId
      xGT <- genMatchToken
      oGT <- genMatchToken
      randomStep <- newEmptyMVar
      let randomCB = putMVar randomStep
      randomSendLocRef <- newIORef (const $ return ())
      randomThid <- fork . forever $ do
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
      sessCfg <- forkMatch
        (_srvTimeoutLimit srv)
        (xUN, xGT, const $ return ())
        (oUN, oGT, randomCB)
        (\_ _ _ -> return ())
        removeSelf
      writeIORef randomSendLocRef (_userCfgSendLoc $ _matchCfgO sessCfg)
      atomically $ modifyTVar (_srvMatches srv) (M.insert matchId sessCfg)
      return $ StartResponse xMatchInfo Users{ _uX = xUN, _uO = oUN } (GameState emptyBoard Nothing)

-- ServerEsque.registerUser :: RegisterRequest -> m (Maybe RegisterResponse)
registerUser :: (MonadIO m, DB m) => Server IO -> RegisterRequest -> m (Maybe RegisterResponse)
registerUser srv rreq = do
  let name@(UserName un) = _rreqName rreq
  userKey <- liftIO genUserKey
  mUsers <- liftIO $ tryRegister srv name userKey
  case mUsers of
    Nothing -> return Nothing
    Just users -> do
      storeUsers users
      return . Just $ RegisterResponse (UserCreds name userKey)
  where
    tryRegister :: Server IO -> UserName -> UserKey -> IO (Maybe (M.Map UserName UserKey))
    tryRegister srv name userKey = atomically $ do
      users <- readTVar (_srvUsers srv)
      let users' = M.insert name userKey users
      if M.member name users
        then return Nothing
        else writeTVar (_srvUsers srv) users' >> return (Just users')
