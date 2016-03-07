{-# OPTIONS -fno-warn-orphans #-}
module T3.Service
  ( GameToken
  , GameId
  , start
  , play
  ) where

import qualified Data.Map as M

import Prelude
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Control.Monad.Except
import System.Random

import T3.Game.Core
import T3.Game
import T3.Game.Types
import T3.Comm.Types
import T3.Service.Dispatch
import T3.Service.Lobby

data Server = Server
  { srvLobby :: TVar [UserId]
  , srvSessions :: TVar (M.Map GameId SessionConfig)
  , srvUsers :: TVar (M.Map UserKey UserId)
  , srvKillSelf :: IO ()
  }

forkServer :: IO Server
forkServer = do
  lobby <- newTVarIO []
  sess <- newTVarIO M.empty
  users <- newTVarIO M.empty
  let srv = Server lobby sess users (return ())
  thid <- forkIO $ serve srv
  return srv { srvKillSelf = killThread thid }

serve :: Server -> IO ()
serve srv = do
  musers <- userPairFromLobby (srvLobby srv)
  case musers of
    Nothing -> return ()
    Just (xUI, oUI) -> do
      let xGT = "x"
      let oGT = "o"
      let gameId = "some ID"
      -- TODO
      sessCfg <- forkSession  (xUI, xGT, const $ return ()) (oUI, oGT, const $ return ()) (\_ _ _ -> return ()) (return ())
      atomically $ modifyTVar (srvSessions srv) (M.insert gameId sessCfg)
  threadDelay (1 * 1000000)
  serve srv

--

type Username = Text
type UserKey = Text
type SessionId = Text

data UserStart = UserStart
  { usKey :: UserKey
  }

data UserPlay = UserPlay
  { upKey :: UserKey
  , upLoc :: Loc
  }

data GameStart = GameStart

data GamePlay = GamePlay

start :: UserStart -> IO GameStart
start userStart = do
  -- addUserToLobby
  -- wait for mvar
  return GameStart

play :: GameId -> GameToken -> UserPlay -> IO GamePlay
play gameId gameToken userPlay = do
  return GamePlay

instance FromJSON UserStart where
  parseJSON (Object o) = UserStart <$> o .: "key"
  parseJSON _ = mzero

instance FromJSON UserPlay where
  parseJSON (Object o) = UserPlay <$> o .: "key" <*> o .: "location"
  parseJSON _ = mzero

instance FromJSON Loc where
  parseJSON (Object o) = Loc <$> o .: "x" <*> o .: "y"
  parseJSON _ = mzero

instance ToJSON GameStart where
  toJSON _ = object []

instance ToJSON GamePlay where
  toJSON _ = object []


