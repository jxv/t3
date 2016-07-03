module T3.Server.Impl
 ( playMove
 , startMatch
 , randomMatch
 , registerUser
 ) where

import Control.Applicative
import Control.Monad.Conc.Class
import Control.Monad.Random
import Control.Monad (mzero, forever)
import Control.Monad.Trans (MonadIO, liftIO)

import T3.Core
import T3.Server hiding (ServerEsque(..))
import T3.Server.Util
import T3.Part
import T3.Lobby
import T3.Storage
import T3.Match
import T3.Util

playMove :: Part m => MatchId -> MatchToken -> PlayRequest -> m (Maybe PlayResponse)
playMove matchId matchToken playReq = do
  mUserCfg <- userConfig matchId matchToken playReq
  case mUserCfg of
    Nothing -> return Nothing
    Just userCfg -> playResponse userCfg playReq

startMatch :: (Lobby m, Part m, MonadConc m) => StartRequest -> m (Maybe StartResponse)
startMatch startReq = do
  resp <- newEmptyMVar
  authenticated <- authenticate (_sreqCreds startReq)
  if not authenticated
    then return Nothing
    else do
      added <- addUserToLobby
        (_ucName $ _sreqCreds startReq)
        (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
      if added
        then do
          sresp <- takeMVar resp
          return $ Just sresp
        else return Nothing

randomMatch :: Part m => StartRequest -> m (Maybe StartResponse)
randomMatch startReq = do
  authenticated <- authenticate (_sreqCreds startReq)
  if not authenticated
    then return Nothing
    else fmap Just (randomResponse startReq)

registerUser :: (Part m, Storage m, MonadRandom m) => RegisterRequest -> m (Maybe RegisterResponse)
registerUser rreq = do
  let name@(UserName un) = _rreqName rreq
  userKey <- genUserKey
  mUsers <- tryRegister name userKey
  case mUsers of
    Nothing -> return Nothing
    Just users -> do
      storeUsers users
      return . Just $ RegisterResponse (UserCreds name userKey)