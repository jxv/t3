{-# OPTIONS -fno-warn-orphans #-}
module T3.Server
  ( GameLogger
  , Server(..)
  , UserCreds(..)
  , StartRequest(..)
  , PlayRequest(..)
  , GameState(..)
  , StartResponse(..)
  , PlayResponse(..)
  , Username
  , UserKey
  , forkServer
  , genBase64
  , genMatchToken
  , genMatchId
  , genUserId
  , genUserKey
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
genMatchToken = genBase64 16

genMatchId :: IO Text
genMatchId = genBase64 16

genUserId :: IO Text
genUserId = genBase64 32

genUserKey :: IO Text
genUserKey = genBase64 64

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

data UserCreds = UserCreds
  { ucUserId :: UserId
  , ucUserKey :: UserKey
  } deriving (Show, Eq)

data StartRequest = StartRequest
  { sreqUserCreds :: UserCreds
  } deriving (Show, Eq)

data PlayRequest = PlayRequest
  { preqUserCreds :: UserCreds
  , preqLoc :: Loc
  } deriving (Show, Eq)

instance FromJSON UserCreds where
  parseJSON (Object o) = UserCreds <$> o .: "id" <*> o .: "key"
  parseJSON _ = mzero

instance FromJSON StartRequest where
  parseJSON (Object o) = StartRequest <$> o .: "creds"
  parseJSON _ = mzero

instance FromJSON PlayRequest where
  parseJSON (Object o) = PlayRequest <$> o .: "creds" <*> o .: "loc"
  parseJSON _ = mzero

instance FromJSON Loc where
  parseJSON (Object o) = Loc <$> o .: "x" <*> o .: "y"
  parseJSON _ = mzero

data GameState = GameState
  { gsBoard :: Board
  , gsFinal :: Maybe Final
  } deriving (Show, Eq)

data StartResponse = StartResponse
  { srespMatchInfo :: MatchInfo
  , srespState :: GameState
  } deriving (Show, Eq)

data PlayResponse = PlayResponse
  { prespState :: GameState
  } deriving (Show, Eq)

instance ToJSON Board where
  toJSON b = toJSON [toJSON [cvt $ M.lookup (Loc x y) m | x <- [0..pred s]] | y <- [0..pred s]]
    where
      m = boardMap b
      s = boardSize b
      cvt :: Maybe XO -> String
      cvt (Just X) = "x"
      cvt (Just O) = "o"
      cvt Nothing = " "

instance ToJSON Final where
  toJSON f = String $ case f of
    Won -> "win"
    WonByDQ -> "win"
    Loss -> "lose"
    LossByDQ -> "lose"
    Tied -> "tie"

instance ToJSON GameState where
  toJSON gs = object [ "board" .= (gsBoard gs), "final" .= (gsFinal gs) ]

instance ToJSON MatchInfo where
  toJSON mi = object [ "id" .= miMatchId mi, "token" .= miMatchToken mi ]

instance ToJSON StartResponse where
  toJSON sreq = object [ "match" .= srespMatchInfo sreq, "state" .= srespState sreq ]

instance ToJSON PlayResponse where
  toJSON preq = object [ "state" .= prespState preq ]
