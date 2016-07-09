{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
module T3.Server.Serve
  ( GameLogger
  , Server(..)
  , UserConfig(..)
  , MatchConfig(..)
  , UserCreds(..)
  , StartRequest(..)
  , PlayRequest(..)
  , GameState(..)
  , Users(..)
  , StartResponse(..)
  , PlayResponse(..)
  , UserName(..)
  , UserKey(..)
  , RegisterRequest(..)
  , RegisterResponse(..)
  , forkServer
  , genBase64
  , genMatchToken
  , genMatchId
  , genUserName
  , genUserKey
  , authenticate
  , authorize
  , toGameState
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Conc.Class
import Control.Concurrent.Classy.STM
import Control.Monad
import Control.Monad.Random
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Options(..), defaultOptions, Parser)
import Data.Text (Text)
import System.Random
import GHC.Generics
import Data.Char

import T3.Game
import T3.Match -- types
import T3.Server hiding (ServerEsque(..)) -- types
import T3.Server.Impl
import T3.Dispatch hiding (Dispatch(..)) -- types
import T3.Dispatch.Impl
import T3.Lobby hiding (Lobby(..)) -- types
import T3.Lobby.Impl
import T3.Part.Impl
import T3.Server.Util
import T3.Util

forkServer :: (MonadConc m, MonadSTM m, MonadRandom m) => GameLogger m -> Maybe Seconds -> M.Map UserName UserKey -> m (Server m)
forkServer logger timeoutLimit users = do
  lobby <- atomically $ newTVar []
  matches <- atomically $ newTVar M.empty
  users <- atomically $ newTVar users
  let srv = Server lobby matches users (return ()) logger timeoutLimit
  thid <- fork $ serve srv
  let killMatches = do
        killers <- atomically $ do
          s <- readTVar matches
          return $ map _matchCfgDie (M.elems s)
        sequence_  killers
  return srv{ _srvDie = killMatches >> killThread thid }

serve :: (MonadConc m, MonadSTM m, MonadRandom m) => Server m -> m ()
serve srv = do
  musers <- userPairFromLobby (_srvLobby srv)
  case musers of
    Nothing -> return ()
    Just ((xUN, xCB), (oUN, oCB)) -> do
      matchId <- genMatchId
      xGT <- genMatchToken
      oGT <- genMatchToken
      let removeSelf = atomically $ modifyTVar (_srvMatches srv) (M.delete matchId)
      let users = Users { _uX = xUN, _uO= oUN }
      let xMatchInfo = MatchInfo matchId xGT 
      let oMatchInfo = MatchInfo matchId oGT
      sessCfg <- forkMatch
        (_srvTimeoutLimit srv)
        (xUN, xGT, xCB xMatchInfo users)
        (oUN, oGT, oCB oMatchInfo users)
        (_srvLogger srv matchId users)
        removeSelf
      atomically $ modifyTVar (_srvMatches srv) (M.insert matchId sessCfg)
  threadDelay (1 * 1000000) -- 1 second
  serve srv