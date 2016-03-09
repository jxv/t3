{-# OPTIONS -fno-warn-orphans #-}
module T3.Server
  ( GameLogger
  , Server(..)
  , forkServer
  , start
  , play
  , genBase64
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

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
import T3.Server.Dispatch
import T3.Server.Lobby
import T3.Match

type GameLogger = MatchId -> Win UserId -> Lose UserId -> Board -> IO ()

data Server = Server
  { srvLobby :: TVar Lobby
  , srvMatches :: TVar (M.Map MatchId MatchConfig)
  , srvUsers :: TVar (M.Map UserKey UserId)
  , srvDie :: IO ()
  , srvLogger :: GameLogger
  }

forkServer :: GameLogger ->  IO Server
forkServer logger = do
  lobby <- newTVarIO []
  matches <- newTVarIO M.empty
  users <- newTVarIO M.empty
  let srv = Server lobby matches users (return ()) logger
  thid <- forkIO $ serve srv
  let killMatches = do
        killers <- atomically $ do
          s <- readTVar matches
          return $ map matchCfgDie (M.elems s)
        sequence_  killers
  return srv { srvDie = killMatches >> killThread thid }

genBase64 :: Int -> IO Text
genBase64 n = fmap T.pack (sequence $ replicate n gen)
  where
    gen = fmap (\x -> vals !! (mod x len)) randomIO
    len = length vals
    vals = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-','_']

genMatchToken :: IO Text
genMatchToken = genBase64 32

genMatchId :: IO Text
genMatchId = genBase64 32

genUserId :: IO Text
genUserId = genBase64 32

serve :: Server -> IO ()
serve srv = do
  musers <- userPairFromLobby (srvLobby srv)
  case musers of
    Nothing -> return ()
    Just ((xUI, xCB), (oUI, oCB)) -> do
      matchId <- genMatchId
      xGT <- genMatchToken
      oGT <- genMatchToken
      let removeSelf = atomically $ modifyTVar (srvMatches srv) (M.delete matchId)
      sessCfg <- forkMatch  (xUI, xGT, xCB matchId xGT) (oUI, oGT, oCB matchId oGT) (srvLogger srv matchId) removeSelf
      atomically $ modifyTVar (srvMatches srv) (M.insert matchId sessCfg)
  threadDelay (1 * 1000000)
  serve srv

--

type Username = Text
type UserKey = Text

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

play :: MatchId -> MatchToken -> UserPlay -> IO GamePlay
play matchId matchToken userPlay = do
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


