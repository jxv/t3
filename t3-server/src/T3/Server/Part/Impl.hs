module T3.Server.Part.Impl
  ( GameLogger
  , Server(..)
  , authenticate
  , playResponse
  , tryRegister
  , userConfig
  , randomResponse
  , authorize
  ) where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Conc.Class
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (mzero, forever)
import Control.Concurrent.Classy.STM
import Control.Concurrent.Async
import Data.IORef

import T3.Core
import T3.Bot.Random
import T3.Server hiding (ServerEsque(..)) -- types
import T3.Server.Util
import T3.Server.Lobby hiding (Lobby(..)) -- types
import T3.Server.Dispatch hiding (Dispatch(..))
import T3.Server.Dispatch.Impl
import T3.Server.Util
import T3.Server (MatchInfo(..))
import T3.Server.Match hiding (Types(..))
import T3.Server.Match.Impl (delay)
import T3.Server.Util

type GameLogger m = MatchId -> Users -> [Action] -> Board -> Result -> m ()

data Server m = Server
  { _srvLobby :: ListLobby m
  , _srvMatches :: TVar (STM m) (M.Map MatchId (MatchConfig m))
  , _srvUsers :: TVar (STM m) (M.Map UserName UserKey)
  , _srvDie :: m ()
  , _srvLogger :: GameLogger m
  , _srvTimeoutLimit :: Maybe Seconds
  }

authenticate :: MonadConc m => Server m -> UserCreds -> STM m Bool
authenticate srv uc = do
  users <- readTVar (_srvUsers srv)
  return $ M.lookup (_ucName uc) users == Just (_ucKey uc)

playResponse :: UserConfig IO -> PlayRequest -> IO (Maybe PlayResponse)
playResponse userCfg playReq = do
  resp <- liftIO newEmptyMVar
  liftIO $ (_userCfgSendLoc userCfg) (_preqLoc playReq, putMVar resp . PlayResponse . toGameState)
  liftIO $ (either id id) <$> race (Just <$> takeMVar resp) (delay (Seconds 60) >> return Nothing)

tryRegister :: Server IO -> UserName -> UserKey -> IO (Maybe (M.Map UserName UserKey))
tryRegister srv name userKey = atomically $ do
  users <- readTVar (_srvUsers srv)
  let users' = M.insert name userKey users
  if M.member name users
    then return Nothing
    else writeTVar (_srvUsers srv) users' >> return (Just users')

userConfig :: Server IO -> MatchId -> MatchToken -> PlayRequest -> IO (Maybe (UserConfig IO))
userConfig srv matchId matchToken playReq = liftIO . atomically $ do
  let creds = _preqCreds playReq
  authenicated <- authenticate srv creds
  if not authenicated
    then return Nothing
    else do
      mMatchCfg <- M.lookup matchId <$> readTVar (_srvMatches srv)
      return $ authorize (_ucName creds) matchToken =<< mMatchCfg

randomResponse :: Server IO -> StartRequest -> IO StartResponse
randomResponse srv startReq = do
  matchId <- genMatchId
  xGT <- genMatchToken
  oGT <- genMatchToken
  let xUN = _ucName (_sreqCreds startReq)
  let oUN = UserName "random"
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
  let removeSelf = do
        killThread randomThid
        atomically $ modifyTVar (_srvMatches srv) (M.delete matchId) 
        return ()
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

authorize :: Monad m => UserName -> MatchToken -> MatchConfig m -> Maybe (UserConfig m)
authorize un mt mc = (userCfgMay $ _matchCfgX mc) <|> (userCfgMay $ _matchCfgO mc)
  where
    userCfgMay cfg =
      if _userCfgUserName cfg == un && _userCfgMatchToken cfg == mt
        then Just cfg
        else Nothing
