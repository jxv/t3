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
import T3.Match
import T3.Util
import T3.Registrar (Registrar(..))

playMove :: Part m => MatchId -> MatchToken -> PlayRequest -> m PlayResponse
playMove matchId matchToken playReq = do
  userCfg <- userConfig matchId matchToken playReq
  playResponse userCfg playReq

startMatch :: (Lobby m, Part m, MonadConc m, Registrar m) => StartRequest -> m StartResponse
startMatch startReq = do
  resp <- newEmptyMVar
  authenticate (_sreqCreds startReq)
  addUserToLobby
    (_ucName $ _sreqCreds startReq)
    (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
  sresp <- takeMVar resp
  return sresp

randomMatch :: (Part m, Registrar m) => StartRequest -> m StartResponse
randomMatch startReq = do
  authenticate (_sreqCreds startReq)
  randomResponse startReq

registerUser :: (Part m, Registrar m, MonadRandom m) => RegisterRequest -> m RegisterResponse
registerUser rreq = do
  let name@(UserName un) = _rreqName rreq
  userKey <- genUserKey
  register name userKey
  -- storeUsers users
  return $ RegisterResponse (UserCreds name userKey)
