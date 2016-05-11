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
import T3.Server.Dispatch.Types
import T3.Server.Dispatch.Impl.MonadConc
import T3.Server.Lobby.Types
import T3.Server.Lobby.Impl.MonadConc
import T3.Match

type GameLogger m = MatchId -> Users -> [Action] -> Board -> Result -> m ()

data Server m = Server
  { _srvLobby :: ListLobby m
  , _srvMatches :: TVar (M.Map MatchId (MatchConfig m))
  , _srvUsers :: TVar (M.Map UserName UserKey)
  , _srvDie :: m ()
  , _srvLogger :: GameLogger m
  , _srvTimeoutLimit :: Maybe Seconds
  }

authenticate :: MonadConc m => Server m -> UserCreds -> STM Bool
authenticate srv uc = do
  users <- readTVar (_srvUsers srv)
  return $ M.lookup (_ucName uc) users == Just (_ucKey uc)

authorize :: UserName -> MatchToken -> MatchConfig m -> Maybe (UserConfig m)
authorize un mt mc = (userCfgMay $ _matchCfgX mc) <|> (userCfgMay $ _matchCfgO mc)
  where
    userCfgMay cfg =
      if _userCfgUserName cfg == un && _userCfgMatchToken cfg == mt
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
          return $ map _matchCfgDie (M.elems s)
        sequence_  killers
  return srv{ _srvDie = killMatches >> killThread thid }

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

toGameState :: Step -> GameState
toGameState s = GameState (_stepBoard s) (_stepFinal s)

--

newtype UserKey = UserKey { getUserKey :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data RegisterRequest = RegisterRequest
  { _rreqName :: UserName
  } deriving (Show, Eq, Generic)

instance FromJSON RegisterRequest where
  parseJSON = dropPrefixP "_rreq"

instance ToJSON RegisterRequest where
  toJSON = dropPrefixJ "_rreq"

data RegisterResponse = RegisterResponse
  { _rrespCreds :: UserCreds
  } deriving (Show, Eq, Generic)

instance FromJSON RegisterResponse where
  parseJSON = dropPrefixP "_rresp"

instance ToJSON RegisterResponse where
  toJSON = dropPrefixJ "_rresp"

data UserCreds = UserCreds
  { _ucName :: UserName
  , _ucKey :: UserKey
  } deriving (Show, Eq, Generic)

instance FromJSON UserCreds where
  parseJSON = dropPrefixP "_uc"

instance ToJSON UserCreds where
  toJSON = dropPrefixJ "_uc"

data StartRequest = StartRequest
  { _sreqCreds :: UserCreds
  } deriving (Show, Eq, Generic)

instance FromJSON StartRequest where
  parseJSON = dropPrefixP "_sreq"

instance ToJSON StartRequest where
  toJSON = dropPrefixJ "_sreq"

data PlayRequest = PlayRequest
  { _preqCreds :: UserCreds
  , _preqLoc :: Loc
  } deriving (Show, Eq, Generic)

instance FromJSON PlayRequest where
  parseJSON = dropPrefixP "_preq"

instance ToJSON PlayRequest where
  toJSON = dropPrefixJ "_preq"

data StartResponse = StartResponse
  { _srespMatchInfo :: MatchInfo
  , _srespUsers :: Users
  , _srespState :: GameState
  } deriving (Show, Eq, Generic)

instance FromJSON StartResponse where
  parseJSON = dropPrefixP "_sresp"

instance ToJSON StartResponse where
  toJSON = dropPrefixJ "_sresp"

data PlayResponse = PlayResponse
  { _prespState :: GameState
  } deriving (Show, Eq, Generic)

instance FromJSON PlayResponse where
  parseJSON = dropPrefixP "_presp"

instance ToJSON PlayResponse where
  toJSON = dropPrefixJ "_presp"

data GameState = GameState
  { _gsBoard :: Board
  , _gsFinal :: Maybe Final
  } deriving (Show, Eq, Generic)

instance FromJSON GameState where
  parseJSON = dropPrefixP "_gs"

instance ToJSON GameState where
  toJSON = dropPrefixJ "_gs"
