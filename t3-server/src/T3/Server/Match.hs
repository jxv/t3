{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module T3.Server.Match
  ( MatchState(..)
  , Connections(..)
  , Callbacks(..)
  , MatchInfoState(..)
  , GameState(..)
  , Match 
  , runMatch
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Monad.STM as IO
import Control.Concurrent.Async (race)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State (StateT(..), MonadState(..), gets, evalStateT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Conc.Class (threadDelay)
import Data.Map (Map)

import T3.Game (Game(..))
import T3.Core (Loc, Action, Board, XO(..))

import qualified T3.Server.ConsoleImpl as Console
import qualified T3.Server.GameImpl as Game
import qualified T3.Server.GameCommImpl as GameComm
import qualified T3.Server.MatchTransmitterImpl as MatchTransmitter
import qualified T3.Server.ConnectionCallbackImpl as ConnectionCallback
import qualified T3.Server.MatchLoggerImpl as MatchLogger
import T3.Server.Connection (Connection)
import T3.Server.Milliseconds (Milliseconds(..))
import T3.Server (Step(..), Users, MatchId)
import T3.Server.GameComm (GameComm(..))
import T3.Server.MatchLogger (MatchLogger(..))
import T3.Server.HasMatchState (HasMatchState(..))
import T3.Server.HasConnection (HasConnection(..))
import T3.Server.MatchTransmitter (MatchTransmitter(..))
import T3.Server.ConnectionCallback (ConnectionCallback(..))
import T3.Server.Stoppable (Stoppable(..))
import T3.Server.HasCallbacks (HasCallbacks(..))
import T3.Server.OnTimeout (OnTimeout(..))
import T3.Server.HasTimeoutLimit (HasTimeoutLimit(..))
import T3.Server.Console (Console(..))
import T3.Server.Storage (Storage(..))
import T3.Server.MatchInfo (MatchInfo(..))

newtype Match a = Match { unMatch :: MaybeT (StateT (MatchState Match) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (MatchState Match))

data MatchState m = MatchState
  { _callbacks :: Callbacks m
  , _timeoutLimit :: Maybe Milliseconds
  , _matchInfoState :: MatchInfoState
  , _gameState :: GameState
  , _connections :: Connections
  }

data Connections = Connections
  { _connectionsX :: Connection
  , _connectionsO :: Connection
  } deriving (Show, Eq)

data Callbacks m = Callbacks
  { _callbacksConnectionMap :: Map Connection (m (Loc, Step -> m ()), Step -> m ())
  }

data MatchInfoState = MatchInfoState
  { _matchInfoUsers :: Users
  , _matchInfoMatchId :: MatchId
  } deriving (Show, Eq)

data GameState = GameState
  { _gameBoard :: Board
  , _gameActions :: [Action]
  } deriving (Show, Eq)

runMatch :: Match a -> MatchState Match -> IO (Maybe a, MatchState Match)
runMatch game st = runStateT (runMaybeT (unMatch game)) st

instance Game Match where
  move = Game.move
  forfeit = Game.forfeit
  end = Game.end
  tie = Game.tie
  step = Game.step

instance GameComm Match where
  sendGameState = GameComm.sendGameState
  recvAction = GameComm.recvAction
  sendFinal = GameComm.sendFinal
  tally = GameComm.tally
  updateBoard = GameComm.updateBoard
  logAction = GameComm.logAction

instance Stoppable Match where
  stop = Match $ MaybeT $ return Nothing

instance HasMatchState Match where
  getBoard = gets (_gameBoard . _gameState)
  putBoard board = do
    matchState <- get
    put $ matchState{ _gameState = (_gameState matchState){ _gameBoard = board } }
  getActions = gets (_gameActions . _gameState)
  appendAction action = do
    matchState <- get
    let gameState = _gameState matchState
    put matchState{ _gameState = gameState{ _gameActions = _gameActions gameState ++ [action] } }

instance HasConnection Match where
  getConnection xo = do
    connections <- gets _connections
    return $ case xo of
      X -> _connectionsX connections
      O -> _connectionsO connections

instance MatchTransmitter Match where
  sendStep = MatchTransmitter.sendStep
  recvLoc = MatchTransmitter.recvLoc

instance ConnectionCallback Match where
  getRequest = ConnectionCallback.getRequest
  getRespond = ConnectionCallback.getRespond
  putRespond = ConnectionCallback.putRespond

instance HasCallbacks Match where
  getCallbacks = gets (_callbacksConnectionMap . _callbacks)
  putCallbacks connectionMap = do
    matchState <- get
    put $ matchState{ _callbacks = (_callbacks matchState){ _callbacksConnectionMap = connectionMap } }

instance MatchLogger Match where
  logMatch = MatchLogger.logMatch

instance MatchInfo Match where
  getUsers = gets (_matchInfoUsers . _matchInfoState)
  getMatchId = gets (_matchInfoMatchId . _matchInfoState)

instance OnTimeout Match where
  onTimeout callee ms = do
    st <- get
    eitherOfMaybeAndMatchState <- liftIO $ race (liftIO $ delay 3000) (liftIO $ runMatch callee st)
    case eitherOfMaybeAndMatchState of
      Left _ -> return Nothing
      Right (maybeA, st') -> do
        put st'
        return maybeA
    where
      delay :: Milliseconds -> IO ()
      delay (Milliseconds ms) = threadDelay (scaleFromNano * ms)
        where
          scaleFromNano = 1000

instance HasTimeoutLimit Match where
  getTimeoutLimit = gets _timeoutLimit

instance Console Match where
  printStdout = Console.printStdout
