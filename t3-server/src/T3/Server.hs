{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Concurrent.STM (TVar, STM, readTVar, modifyTVar)
import Control.Monad.Conc.ClassTmp
import Control.Monad
import Control.Monad.Random
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Options(..), defaultOptions, Parser)
import Data.Text (Text)
import System.Random
import GHC.Generics
import Data.Char

import T3.Game
import T3.Server.Dispatch
import T3.Server.Lobby
import T3.Match

type GameLogger m = MatchId -> Users -> [Action] -> Board -> Result -> m ()

data Server m = Server
  { srvLobby :: TVar (Lobby m)
  , srvMatches :: TVar (M.Map MatchId (MatchConfig m))
  , srvUsers :: TVar (M.Map UserName UserKey)
  , srvDie :: m ()
  , srvLogger :: GameLogger m
  , srvTimeoutLimit :: Maybe Seconds
  }

authenticate :: MonadConc m => Server m -> UserCreds -> STM Bool
authenticate srv uc = do
  users <- readTVar (srvUsers srv)
  return $ M.lookup (ucName uc) users == Just (ucKey uc)

authorize :: UserName -> MatchToken -> MatchConfig m -> Maybe (UserConfig m)
authorize userName matchToken matchCfg = (userCfgMay $ matchCfgX matchCfg) <|> (userCfgMay $ matchCfgO matchCfg)
  where
    userCfgMay cfg =
      if userCfgUserName cfg == userName && userCfgMatchToken cfg == matchToken
      then Just cfg
      else Nothing

forkServer :: (MonadConc m, MonadRandom m) => GameLogger m -> Maybe Seconds -> M.Map UserName UserKey -> m (Server m)
forkServer logger timeoutLimit users = do
  lobby <- newTVarIO []
  matches <- newTVarIO M.empty
  users <- newTVarIO users
  let srv = Server lobby matches users (return ()) logger timeoutLimit
  thid <- fork $ serve srv
  let killMatches = do
        killers <- atomically $ do
          s <- readTVar matches
          return $ map matchCfgDie (M.elems s)
        sequence_  killers
  return srv{ srvDie = killMatches >> killThread thid }

genBase64 :: MonadRandom m => Int -> m Text
genBase64 n = fmap T.pack (sequence $ replicate n gen)
  where
    gen = fmap (\x -> vals !! (mod x len)) getRandom
    len = length vals
    vals = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-','_']

genMatchToken :: MonadRandom m => m MatchToken
genMatchToken = MatchToken <$> genBase64 16

genMatchId :: MonadRandom m => m MatchId
genMatchId = MatchId <$> genBase64 16

genUserName :: MonadRandom m => m UserName
genUserName = UserName <$> genBase64 32

genUserKey :: MonadRandom m => m UserKey
genUserKey = UserKey <$> genBase64 32

serve :: (MonadConc m, MonadRandom m) => Server m -> m ()
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
        (srvTimeoutLimit srv)
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

newtype UserKey = UserKey Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data RegisterRequest = RegisterRequest
  { rreqName :: UserName
  } deriving (Show, Eq, Generic)

instance FromJSON RegisterRequest where
  parseJSON = dropPrefixP "rreq"

instance ToJSON RegisterRequest where
  toJSON = dropPrefixJ "rreq"

data RegisterResponse = RegisterResponse
  { rrespCreds :: UserCreds
  } deriving (Show, Eq, Generic)

instance FromJSON RegisterResponse where
  parseJSON = dropPrefixP "rresp"

instance ToJSON RegisterResponse where
  toJSON = dropPrefixJ "rresp"

data UserCreds = UserCreds
  { ucName :: UserName
  , ucKey :: UserKey
  } deriving (Show, Eq, Generic)

instance FromJSON UserCreds where
  parseJSON = dropPrefixP "uc"

instance ToJSON UserCreds where
  toJSON = dropPrefixJ "uc"

data StartRequest = StartRequest
  { sreqCreds :: UserCreds
  } deriving (Show, Eq, Generic)

instance FromJSON StartRequest where
  parseJSON = dropPrefixP "sreq"

instance ToJSON StartRequest where
  toJSON = dropPrefixJ "sreq"

data PlayRequest = PlayRequest
  { preqCreds :: UserCreds
  , preqLoc :: Loc
  } deriving (Show, Eq, Generic)

instance FromJSON PlayRequest where
  parseJSON = dropPrefixP "preq"

instance ToJSON PlayRequest where
  toJSON = dropPrefixJ "preq"

data StartResponse = StartResponse
  { srespMatchInfo :: MatchInfo
  , srespUsers :: Users
  , srespState :: GameState
  } deriving (Show, Eq, Generic)

instance FromJSON StartResponse where
  parseJSON = dropPrefixP "sresp"

instance ToJSON StartResponse where
  toJSON = dropPrefixJ "sresp"

data PlayResponse = PlayResponse
  { prespState :: GameState
  } deriving (Show, Eq, Generic)

instance FromJSON PlayResponse where
  parseJSON = dropPrefixP "presp"

instance ToJSON PlayResponse where
  toJSON = dropPrefixJ "presp"

data GameState = GameState
  { gsBoard :: Board
  , gsFinal :: Maybe Final
  } deriving (Show, Eq, Generic)

instance FromJSON GameState where
  parseJSON = dropPrefixP "gs"

instance ToJSON GameState where
  toJSON = dropPrefixJ "gs"
