{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module T3.Server.GameInstance
  (
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
import qualified T3.Server.MatchImpl as Match
import qualified T3.Server.MatchTransmitterImpl as MatchTransmitter
import qualified T3.Server.ConnectionCallbackImpl as ConnectionCallback
import qualified T3.Server.MatchLoggerImpl as MatchLogger
import T3.Server.Connection (Connection)
import T3.Server.Milliseconds (Milliseconds(..))
import T3.Server (Step(..), Users, MatchId)
import T3.Server.Match (Match(..))
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

data GameState m = GameState
  { _gameStateCallbacks :: Callbacks m
  , _gameStateTimeoutLimit :: Maybe Milliseconds
  , _gameStateMatchInfo :: MatchInfoData
  , _gameStateMatchState :: MatchStateData
  , _gameStateConnections :: Connections
  }

data Connections = Connections
  { _connectionsX :: Connection
  , _connectionsO :: Connection
  } deriving (Show, Eq)

data Callbacks m = Callbacks
  { _callbacksConnectionMap :: Map Connection (m (Loc, Step -> m ()), Step -> m ())
  }

data MatchInfoData = MatchInfoData
  { _matchInfoUsers :: Users
  , _matchInfoMatchId :: MatchId
  } deriving (Show, Eq)

data MatchStateData = MatchStateData
  { _matchStateBoard :: Board
  , _matchStateActions :: [Action]
  } deriving (Show, Eq)

newtype GameM a = GameM { unGameM :: MaybeT (StateT (GameState GameM) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (GameState GameM))

runGameM :: GameM a -> GameState GameM -> IO (Maybe a, GameState GameM)
runGameM game st = runStateT (runMaybeT (unGameM game)) st

instance Game GameM where
  move = Game.move
  forfeit = Game.forfeit
  end = Game.end
  tie = Game.tie
  step = Game.step

instance Match GameM where
  sendGameState = Match.sendGameState
  recvAction = Match.recvAction
  sendFinal = Match.sendFinal
  tally = Match.tally
  updateBoard = Match.updateBoard
  logAction = Match.logAction

instance Stoppable GameM where
  stop = GameM $ MaybeT $ return Nothing

instance HasMatchState GameM where
  getBoard = gets (_matchStateBoard . _gameStateMatchState)
  putBoard board = do
    gameState <- get
    put $ gameState{ _gameStateMatchState = (_gameStateMatchState gameState){ _matchStateBoard = board } }
  getActions = gets (_matchStateActions . _gameStateMatchState)
  appendAction action = do
    gameState <- get
    let matchState = _gameStateMatchState gameState
    put gameState{ _gameStateMatchState = matchState{ _matchStateActions = _matchStateActions matchState ++ [action] } }

instance HasConnection GameM where
  getConnection xo = do
    connections <- gets _gameStateConnections
    return $ case xo of
      X -> _connectionsX connections
      O -> _connectionsO connections

instance MatchTransmitter GameM where
  sendStep = MatchTransmitter.sendStep
  recvLoc = MatchTransmitter.recvLoc

instance ConnectionCallback GameM where
  getRequest = ConnectionCallback.getRequest
  getRespond = ConnectionCallback.getRespond
  putRespond = ConnectionCallback.putRespond

instance HasCallbacks GameM where
  getCallbacks = gets (_callbacksConnectionMap . _gameStateCallbacks)
  putCallbacks connectionMap = do
    gameState <- get
    put $ gameState{ _gameStateCallbacks = (_gameStateCallbacks gameState){ _callbacksConnectionMap = connectionMap } }

instance MatchLogger GameM where
  logMatch = MatchLogger.logMatch

instance MatchInfo GameM where
  getUsers = gets (_matchInfoUsers . _gameStateMatchInfo)
  getMatchId = gets (_matchInfoMatchId . _gameStateMatchInfo)

instance OnTimeout GameM where
  onTimeout callee ms = do
    st <- get
    eitherOfMaybeAndGameState <- liftIO $ race (liftIO $ delay 3000) (liftIO $ runGameM callee st)
    case eitherOfMaybeAndGameState of
      Left _ -> return Nothing
      Right (maybeA, st') -> do
        put st'
        return maybeA
    where
      delay :: Milliseconds -> IO ()
      delay (Milliseconds ms) = threadDelay (scaleFromNano * ms)
        where
          scaleFromNano = 1000

instance HasTimeoutLimit GameM where
  getTimeoutLimit = gets _gameStateTimeoutLimit

instance Console GameM where
  printStdout = Console.printStdout
