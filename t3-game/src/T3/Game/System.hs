module T3.Game.System
  ( Env(..)
  , Callbacks(..)
  , System
  , io
  ) where

import Control.Monad.State (StateT(..), MonadState(..), gets, evalStateT)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), asks)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (void)

import qualified T3.Game.BoardManagerImpl as BoardManager (isOpenLoc, getResult, insertAtLoc)
import T3.Core (Loc, Action, Board, Result, XO(..), emptyBoard)
import T3.Game.Run (run)
import T3.Game.Parts (Control(..), HasBoard(..), BoardManager(..))

import qualified T3.Game.ControlImpl as Control
import qualified T3.Game.CommunicatorImpl as Communicator
import T3.Game.Types (Step)
import T3.Game.Parts (Communicator(..), Transmitter(..))

newtype System a = System { unSystem :: ReaderT Env (StateT Board IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadState Board)

data Env = Env
  { _callbacks :: XO -> Callbacks
  }

data Callbacks = Callbacks
  { _callbacksRecv :: IO Loc
  , _callbacksSend :: Step -> IO ()
  }

io :: System () -> Env -> IO ()
io system env = void $ runStateT (runReaderT (unSystem system) env) emptyBoard

instance Control System where
  move = Control.move
  forfeit = Control.forfeit
  end = Control.end
  tie = Control.tie

instance BoardManager System where
  isOpenLoc = BoardManager.isOpenLoc
  insertAtLoc = BoardManager.insertAtLoc
  getResult = BoardManager.getResult

instance Communicator System where
  sendGameState = Communicator.sendGameState
  recvAction = Communicator.recvAction
  sendFinal = Communicator.sendFinal

instance HasBoard System where
  getBoard = get
  putBoard = put

instance Transmitter System where
  sendStep xo step = do
    send <- asks (_callbacksSend . flip _callbacks xo)
    liftIO $ send step
  recvLoc xo = do
    recv <- asks (_callbacksRecv . flip _callbacks xo)
    liftIO recv
