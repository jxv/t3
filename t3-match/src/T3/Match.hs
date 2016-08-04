{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T3.Match
  ( MatchState(..)
  , Callbacks(..)
  , GameState(..)
  , Match 
  , runMatch
  , startMatch
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Monad.STM as IO
import Control.Concurrent.Async (race)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State (StateT(..), MonadState(..), gets, evalStateT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map (Map)
import Data.Functor (void)

import T3.Game (Game(..))
import T3.Game.Run (run)
import T3.Core (Loc, Action, Board, Result, XO(..), emptyBoard)

import qualified T3.Match.ConsoleImpl as Console
import qualified T3.Match.GameImpl as Game
import qualified T3.Match.GameCommImpl as GameComm
import T3.Match.Types (Step(..), Users, MatchId)
import T3.Match.Milliseconds (Milliseconds(..), delay)
import T3.Match.GameComm (GameComm(..))
import T3.Match.MatchLogger (MatchLogger(..))
import T3.Match.HasMatchState (HasMatchState(..))
import T3.Match.MatchTransmitter (MatchTransmitter(..))
import T3.Match.Stoppable (Stoppable(..))
import T3.Match.OnTimeout (OnTimeout(..))
import T3.Match.HasTimeoutLimit (HasTimeoutLimit(..))
import T3.Match.Console (Console(..))

newtype Match a = Match { unMatch :: MaybeT (StateT MatchState IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MatchState)

data MatchState = MatchState
  { _callbacks :: XO -> Callbacks
  , _timeoutLimit :: Maybe Milliseconds
  , _gameState :: GameState
  , _logger :: [Action] -> Board -> Result -> IO ()
  }

data Callbacks = Callbacks
  { _callbacksRecv :: IO Loc
  , _callbacksSend :: Step -> IO ()
  }

data GameState = GameState
  { _gameBoard :: Board
  , _gameActions :: [Action]
  } deriving (Show, Eq)

runMatch :: Match a -> MatchState -> IO (Maybe a, MatchState)
runMatch game st = runStateT (runMaybeT (unMatch game)) st

startMatch :: MatchState -> IO ()
startMatch = void . runMatch (run emptyBoard)

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

instance MatchTransmitter Match where
  sendStep xo step = do
    send <- gets (_callbacksSend . flip _callbacks xo)
    liftIO $ send step
  recvLoc xo = do
    recv <- gets (_callbacksRecv . flip _callbacks xo)
    liftIO recv

instance MatchLogger Match where
  logMatch actions board result = do
    logger <- gets _logger
    liftIO $ logger actions board result

instance OnTimeout Match where
  onTimeout callee ms = do
    st <- get
    eitherOfMaybeAndMatchState <- liftIO $ race (liftIO $ delay ms) (liftIO $ runMatch callee st)
    case eitherOfMaybeAndMatchState of
      Left _ -> return Nothing
      Right (maybeA, st') -> do
        put st'
        return maybeA

instance HasTimeoutLimit Match where
  getTimeoutLimit = gets _timeoutLimit

instance Console Match where
  printStdout = Console.printStdout
