module T3.Game.Monad
  ( Env(..)
  , Callbacks(..)
  , Game
  , runIO
  ) where

import Control.Monad.State (StateT(..), MonadState(..), gets, evalStateT)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), asks)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (void)

import T3.Core (Loc, Action, Board, Result, XO(..))

import qualified T3.Game.PlayImpl as Play
import qualified T3.Game.BoardManagerImpl as BoardManager
import qualified T3.Game.ControlImpl as Control
import qualified T3.Game.CommunicatorImpl as Communicator
import T3.Game.Classes
import T3.Game.Types

newtype Game a = Game { unGame :: ReaderT Env (StateT Board IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadState Board)

data Env = Env
  { _callbacks :: XO -> Callbacks
  }

data Callbacks = Callbacks
  { _callbacksRecv :: IO Loc
  , _callbacksSend :: Step -> IO ()
  }

runIO :: Game () -> Env -> Board -> IO ()
runIO game env board = void $ runStateT (runReaderT (unGame game) env) board

instance Play Game where
  play = Play.play'

instance Control Game where
  move = Control.move
  forfeit = Control.forfeit
  end = Control.end
  tie = Control.tie

instance BoardManager Game where
  isOpenLoc = BoardManager.isOpenLoc
  insertAtLoc = BoardManager.insertAtLoc
  getResult = BoardManager.getResult

instance Communicator Game where
  sendGameState = Communicator.sendGameState
  recvAction = Communicator.recvAction
  sendFinal = Communicator.sendFinal

instance HasBoard Game where
  getBoard = get
  putBoard = put

instance Transmitter Game where
  sendStep xo step = do
    send <- asks (_callbacksSend . flip _callbacks xo)
    liftIO $ send step
  recvLoc xo = do
    recv <- asks (_callbacksRecv . flip _callbacks xo)
    liftIO recv
