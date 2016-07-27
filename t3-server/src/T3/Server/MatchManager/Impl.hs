module T3.Server.MatchManager.Impl
  ( -- ( dispatchMatch
  -- , dispatchMatchWithRandomBot
  -- , getUsers
  -- , sendLocation
  -- , killMatch
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Concurrent.Classy.Chan (writeChan, readChan, newChan)
import Control.Concurrent.Classy.STM (TVar)
import Control.Monad.Conc.Class (STM, MonadConc, fork, atomically, ThreadId)
import Data.Map (Map)
import T3.Core (Loc(..), XO, Board)
import T3.Game (Win, Lose, Game(..))
import T3.Game.Run (run)
import T3.Server (MatchId(..), UserName(..), Users(..), Step(..), UserInit)
import T3.Server.Util ()

{-
data UserConfig m = UserConfig
  { _userCfgUserName :: UserName
  , _userCfgSendLoc :: (Loc, Step -> m ()) -> m ()
  }

data MatchConfig m = MatchConfig
  { _matchCfgX :: UserConfig m
  , _matchCfgO :: UserConfig m
  }

newtype Connection = Connection (MatchId, UserName)

type Matches m = Map MatchId (MatchConfig m)
type MatchKillers m = Map MatchId (m ())

data GameRecord m = GameRecord
  { _gameRecordMove :: m Loc
  , _gameRecordForfeit :: Win XO -> Lose XO -> m ()
  , _gameRecordEnd :: Win XO -> Lose XO -> m ()
  , _gameRecordTie :: m ()
  , _gameRecordStep :: Board -> XO -> Loc -> m ()
  }

class Monad m => Alpha m where
  genMatchId :: m MatchId
  genGameRecord :: Connection -> Connection -> m (GameRecord m)
  noMatchId :: m a
  noUserName :: m a

newtype GameT m a = GameT { unGameT :: ReaderT (GameRecord m) m a }

runGame :: Monad m => GameT m () -> GameRecord m -> m ()
runGame stack record = runReaderT (unGameT stack) record

dispatchMatch
  :: (MonadConc m, Alpha m)
  => TVar (STM m) (Matches m)
  -> TVar (STM m) (MatchKillers m)
  -> (UserName, Connection)
  -> (UserName, Connection)
  -> m MatchId
dispatchMatch tvarMatches tvarMatchKillers (xUserName, xConnection) (oUserName, oConnection) = do
  xChan <- newChan
  oChan <- newChan
  let x = (xConnection, readChan xChan)
  let o = (oConnection, readChan oChan)
  matchId <- genMatchId
  let matchConfig = MatchConfig
        { _matchCfgX = UserConfig { _userCfgUserName = xUserName, _userCfgSendLoc = writeChan xChan }
        , _matchCfgO = UserConfig { _userCfgUserName = oUserName, _userCfgSendLoc = writeChan oChan }
        }
  -- insert match config
  atomically $ do
    return ()
  thid <- forkGame x o
  -- kill funciton thread here? 
  return matchId

forkGame :: (MonadConc m) => Connection -> Connection -> m (ThreadId m)
forkGame x o = fork $ do
  gameRecord <- genGameRecord x o
  runGame (run emptyBoard) gameRecord

dispatchMatchWithRandomBot :: Monad m => UserName -> m MatchId
dispatchMatchWithRandomBot userName = do
  error "nyi"

getUsers :: Monad m => MatchId -> m Users
getUsers matchId = do
  error "nyi"

sendLocation :: Alpha m => MatchId -> UserName -> Loc -> m ()
sendLocation matchId userName loc = do
  noMatchId

killMatch :: Alpha m => MatchId -> m ()
killMatch matchId = do
  noMatchId
  -}
