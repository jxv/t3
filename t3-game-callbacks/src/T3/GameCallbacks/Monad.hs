module T3.GameCallbacks.Monad
  ( Env(..)
  , Callbacks(..)
  , GameCallbacks
  , runIO
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Monad.STM as IO
import Control.Concurrent.Async (race)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State (StateT(..), MonadState(..), gets, evalStateT)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), asks)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (void)

import T3.Game.Monad (GameT, runGameT)

import qualified T3.Game.BoardManager as BoardManager (isOpenLoc, getResult)
import T3.Core (Loc, Action, Board, Result, XO(..), emptyBoard)
import T3.Game.Classes (Control(..), HasBoard(..), BoardManager(..))

import qualified T3.GameCallbacks.Console as Console
import qualified T3.GameCallbacks.Control as Control
import qualified T3.GameCallbacks.Communicator as Communicator
import qualified T3.GameCallbacks.BoardManager as BoardManager
import T3.GameCallbacks.Types
import T3.GameCallbacks.Milliseconds (Milliseconds(..), delay)
import T3.GameCallbacks.Classes

newtype GameCallbacks a = GameCallbacks
  { unGameCallbacks :: MaybeT (ReaderT (Env IO) (StateT Data (GameT IO))) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env IO), MonadState Data)

data Env m = Env
  { _callbacks :: XO -> Callbacks m
  , _timeoutLimit :: Maybe Milliseconds
  , _finalize :: [Action] -> Board -> Result -> m ()
  }

data Callbacks m = Callbacks
  { _callbacksRecv :: m Loc
  , _callbacksSend :: Step -> m ()
  }

data Data = Data
  { _board :: Board
  , _actions :: [Action]
  } deriving (Show, Eq)

go :: GameCallbacks a -> Env IO -> Data -> Board -> IO (Maybe a, Data)
go (GameCallbacks m) env dat board = runGameT (runStateT (runReaderT (runMaybeT m) env) dat) board

runIO :: GameCallbacks a -> Env IO -> IO ()
runIO gameCallbacks env = void $ go gameCallbacks env Data{ _board = emptyBoard, _actions = [] } emptyBoard

instance Control GameCallbacks where
  move = Control.move
  forfeit = Control.forfeit
  end = Control.end
  tie = Control.tie

instance BoardManager GameCallbacks where
  isOpenLoc = BoardManager.isOpenLoc
  insertAtLoc = BoardManager.insertAtLoc
  getResult = BoardManager.getResult

instance Communicator GameCallbacks where
  sendGameState = Communicator.sendGameState
  recvAction = Communicator.recvAction
  sendFinal = Communicator.sendFinal
  tally = Communicator.tally
  updateBoard = Communicator.updateBoard
  logAction = Communicator.logAction

instance Stoppable GameCallbacks where
  stop = GameCallbacks . MaybeT $ return Nothing

instance HasBoard GameCallbacks where
  getBoard = gets _board
  putBoard board = get >>= \dat -> put dat{ _board = board }

instance HasActions GameCallbacks where
  getActions = gets _actions
  putActions actions = get >>= \matchState -> put matchState{ _actions = actions }

instance Transmitter GameCallbacks where
  sendStep xo step = asks (_callbacksSend . flip _callbacks xo) >>= \send -> liftIO $ send step
  recvLoc xo = asks (_callbacksRecv . flip _callbacks xo) >>= \recv -> liftIO recv

instance Finalizer GameCallbacks where
  finalize actions board result = asks _finalize >>= \f -> liftIO $ f actions board result

instance OnTimeout GameCallbacks where
  onTimeout gameCallbacks ms = do
    env <- ask
    dat <- get
    board <- getBoard
    eitherOfMaybeAndData <- liftIO $ race (liftIO $ delay ms) (liftIO $ go gameCallbacks env dat board)
    case eitherOfMaybeAndData of
      Left _ -> return Nothing
      Right (maybeA, dat') -> do
        put dat'
        return maybeA

instance HasTimeoutLimit GameCallbacks where
  getTimeoutLimit = asks _timeoutLimit

instance Console GameCallbacks where
  printStdout = Console.printStdout
