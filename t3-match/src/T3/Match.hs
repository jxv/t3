{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T3.Match
  ( MatchEnv(..)
  , Callbacks(..)
  , MatchState(..)
  , Match 
  , runMatch
  , startMatch
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Monad.STM as IO
import Control.Concurrent.Async (race)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State (StateT(..), MonadState(..), gets, evalStateT)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), asks)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map (Map)
import Data.Functor (void)

import T3.Game (Game(..))
import T3.Game.Run (run)
import T3.Core (Loc, Action, Board, Result, XO(..), emptyBoard)

import qualified T3.Match.ConsoleImpl as Console
import qualified T3.Match.GameImpl as Game
import qualified T3.Match.GameCommImpl as GameComm
import T3.Match.Types (Step(..))
import T3.Match.Milliseconds (Milliseconds(..), delay)
import T3.Match.GameComm (GameComm(..))
import T3.Match.MatchLogger (MatchLogger(..))
import T3.Match.HasMatchState (HasMatchState(..))
import T3.Match.MatchTransmitter (MatchTransmitter(..))
import T3.Match.Stoppable (Stoppable(..))
import T3.Match.OnTimeout (OnTimeout(..))
import T3.Match.HasTimeoutLimit (HasTimeoutLimit(..))
import T3.Match.Console (Console(..))

newtype Match a = Match { unMatch :: MaybeT (ReaderT MatchEnv (StateT MatchState IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader MatchEnv, MonadState MatchState)

data MatchEnv = MatchEnv
  { _callbacks :: XO -> Callbacks
  , _timeoutLimit :: Maybe Milliseconds
  , _logger :: [Action] -> Board -> Result -> IO ()
  }

data Callbacks = Callbacks
  { _callbacksRecv :: IO Loc
  , _callbacksSend :: Step -> IO ()
  }

data MatchState = MatchState
  { _board :: Board
  , _actions :: [Action]
  } deriving (Show, Eq)

runMatch :: Match a -> MatchState -> MatchEnv -> IO (Maybe a, MatchState)
runMatch game st env = runStateT (runReaderT (runMaybeT (unMatch game)) env) st

startMatch :: MatchEnv -> IO ()
startMatch = void . runMatch (run emptyBoard) MatchState{ _board = emptyBoard, _actions = [] }

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
  getBoard = gets _board
  putBoard board = do
    matchState <- get
    put $ matchState{ _board = board }
  getActions = gets _actions
  appendAction action = do
    matchState <- get
    put matchState{ _actions = _actions matchState ++ [action] }

instance MatchTransmitter Match where
  sendStep xo step = do
    send <- asks (_callbacksSend . flip _callbacks xo)
    liftIO $ send step
  recvLoc xo = do
    recv <- asks (_callbacksRecv . flip _callbacks xo)
    liftIO recv

instance MatchLogger Match where
  logMatch actions board result = do
    logger <- asks _logger
    liftIO $ logger actions board result

instance OnTimeout Match where
  onTimeout callee ms = do
    env <- ask
    st <- get
    eitherOfMaybeAndMatchState <- liftIO $ race (liftIO $ delay ms) (liftIO $ runMatch callee st env)
    case eitherOfMaybeAndMatchState of
      Left _ -> return Nothing
      Right (maybeA, st') -> do
        put st'
        return maybeA

instance HasTimeoutLimit Match where
  getTimeoutLimit = asks _timeoutLimit

instance Console Match where
  printStdout = Console.printStdout
