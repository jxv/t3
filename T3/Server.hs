{-# OPTIONS -fno-warn-orphans #-}
module T3.Server
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
  , UserName
  , UserKey
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

import Prelude
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Text (Text)
import System.Random

import T3.Game
import T3.Server.Dispatch
import T3.Server.Lobby
import T3.Match

type GameLogger = MatchId -> Users -> [(XO, Loc)] -> Board -> Result -> IO ()

data Server = Server
  { srvLobby :: TVar Lobby
  , srvMatches :: TVar (M.Map MatchId MatchConfig)
  , srvUsers :: TVar (M.Map UserName UserKey)
  , srvDie :: IO ()
  , srvLogger :: GameLogger
  }

authenticate :: Server -> UserCreds -> STM Bool
authenticate srv uc = do
  users <- readTVar (srvUsers srv)
  return $ M.lookup (ucUserName uc) users == Just (ucUserKey uc)

authorize :: UserName -> MatchToken -> MatchConfig -> Maybe UserConfig
authorize userName matchToken matchCfg = (userCfgMay $ matchCfgX matchCfg) <|> (userCfgMay $ matchCfgO matchCfg)
  where
    userCfgMay cfg =
      if userCfgUserName cfg == userName && userCfgMatchToken cfg == matchToken
      then Just cfg
      else Nothing

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

genUserName :: IO Text
genUserName = genBase64 32

genUserKey :: IO Text
genUserKey = genBase64 32

serve :: Server -> IO ()
serve srv = do
  musers <- userPairFromLobby (srvLobby srv)
  case musers of
    Nothing -> return ()
    Just ((xUN, xCB), (oUN, oCB)) -> do
      matchId <- genMatchId
      xGT <- genMatchToken
      oGT <- genMatchToken
      let removeSelf = atomically $ modifyTVar (srvMatches srv) (M.delete matchId)
      let users = Users { uX = xUN, uO = oUN }
      let xMatchInfo = MatchInfo { miMatchId = matchId, miMatchToken = xGT }
      let oMatchInfo = MatchInfo { miMatchId = matchId, miMatchToken = oGT }
      sessCfg <- forkMatch
        (xUN, xGT, xCB xMatchInfo users)
        (oUN, oGT, oCB oMatchInfo users)
        (srvLogger srv matchId users)
        removeSelf
      atomically $ modifyTVar (srvMatches srv) (M.insert matchId sessCfg)
  threadDelay (1 * 1000000) -- 1 second
  serve srv

toGameState :: Step -> GameState
toGameState s = GameState (stepBoard s) (stepFinal s)

--

type UserKey = Text

data UserCreds = UserCreds
  { ucUserName :: UserName
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
  parseJSON (Object o) = UserCreds <$> o .: "name" <*> o .: "key"
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
  , srespUsers :: Users
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
  toJSON gs = object [ "board" .= gsBoard gs, "final" .= gsFinal gs ]

instance ToJSON Users where
  toJSON u = object [ "x" .= uX u, "o" .= uO u ]

instance ToJSON MatchInfo where
  toJSON mi = object [ "id" .= miMatchId mi, "token" .= miMatchToken mi ]

instance ToJSON StartResponse where
  toJSON sreq = object [ "match" .= srespMatchInfo sreq, "users" .= srespUsers sreq, "state" .= srespState sreq ]

instance ToJSON PlayResponse where
  toJSON preq = object [ "state" .= prespState preq ]
