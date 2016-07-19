module T3.Server.Part.Impl
  ( GameLogger
  , Server(..)
  , playResponse
  , randomResponse
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
import T3.Server hiding (Server(..)) -- types
import T3.Server.Lobby hiding (Lobby(..)) -- types
import T3.Server.Dispatch hiding (Dispatch(..))
import T3.Server.Dispatch.Impl
import T3.Server (MatchInfo(..))
import T3.Server.Match hiding (Types(..))
import T3.Server.Match.Impl (delay)
import T3.Server.Util

type GameLogger m = MatchId -> Users -> [Action] -> Board -> Result -> m ()

data Server m = Server
  { _srvLobby :: TVar (STM m) [(UserName, StartCallback m)]
  , _srvMatches :: TVar (STM m) (M.Map MatchId (MatchConfig m))
  , _srvUsers :: TVar (STM m) (M.Map UserName UserKey)
  , _srvDie :: m ()
  , _srvLogger :: GameLogger m
  , _srvTimeoutLimit :: Maybe Seconds
  }

class Monad m => UserMove m where
  move :: UserName -> Loc -> m Step

playResponse :: UserMove m => PlayRequest -> m PlayResponse
playResponse preq = do
  step <- move (_ucName $ _preqCreds preq) (_preqLoc preq)
  return $ PlayResponse (toGameState step)

{- -- BEST BY: UserMove Impl
playResponse :: UserConfig IO -> PlayRequest -> IO (Maybe PlayResponse)
playResponse userCfg playReq = do
  resp <- liftIO newEmptyMVar
  let loc = _preqLoc playReq
  let callback = putMVar resp . PlayResponse . toGameState
  _userCfgSendLoc userCfg (loc, callback)
  (either id id) <$> race (Just <$> takeMVar resp) (delay (Seconds 60) >> return Nothing)

authorize :: Monad m => UserName -> MatchToken -> MatchConfig m -> Maybe (UserConfig m)
authorize un mt mc = (userCfgMay $ _matchCfgX mc) <|> (userCfgMay $ _matchCfgO mc)
  where
    userCfgMay cfg =
      if _userCfgUserName cfg == un && _userCfgMatchToken cfg == mt
        then Just cfg
        else Nothing

userConfig :: Server IO -> MatchId -> MatchToken -> UserCreds -> IO (Maybe (UserConfig IO))
userConfig srv matchId matchToken creds = liftIO . atomically $ do
  mMatchCfg <- M.lookup matchId <$> readTVar (_srvMatches srv)
  return $ authorize (_ucName creds) matchToken =<< mMatchCfg
-}

randomResponse :: Server IO -> StartRequest -> IO StartResponse
randomResponse srv startReq = do
  matchId <- genMatchId
  xGT <- genMatchToken
  oGT <- genMatchToken
  let xUN = _ucName (_sreqCreds startReq)
  let oUN = UserName "random"
  let users = Users { _uX = xUN, _uO = oUN }

  -- ref for send location callback, currently does nothing
  -- There's a cyclic dependency between the callback usage and creation, thus the IORef usage.
  randomSendLocRef <- newIORef (const $ return ())

  -- fork off random bot, get back callback and thread id
  (randomCB, randomThid) <- do
    randomStep <- newEmptyMVar
    let cb = putMVar randomStep
    thid <- fork . forever $ do
      step <- takeMVar randomStep
      mLoc <- randomLoc (_stepBoard step)
      case mLoc of
        Nothing -> return ()
        Just loc -> do
          sendLoc <- readIORef randomSendLocRef
          sendLoc (loc, cb)
    return (cb, thid)

  -- callback to remove match from insertion
  let removeSelf = do
        killThread randomThid
        atomically $ modifyTVar (_srvMatches srv) (M.delete matchId)
        return ()

  -- start and fork match, get callback structure
  sessCfg <- forkMatch
    (_srvTimeoutLimit srv)
    (xUN, xGT, const $ return ())
    (oUN, oGT, randomCB)
    (\_ _ _ -> return ())
    removeSelf

  -- set, send location callback
  writeIORef randomSendLocRef (_userCfgSendLoc $ _matchCfgO sessCfg)

  -- insert match
  atomically $ modifyTVar (_srvMatches srv) (M.insert matchId sessCfg)

  -- response for user
  let xMatchInfo = MatchInfo matchId xGT
  return $ StartResponse xMatchInfo Users{ _uX = xUN, _uO = oUN } (GameState emptyBoard Nothing)
