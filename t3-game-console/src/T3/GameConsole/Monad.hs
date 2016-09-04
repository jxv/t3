module T3.GameConsole.Monad
  ( GameConsole
  , io
  ) where

import Prelude hiding (putStrLn, getLine)
import Control.Monad.State (StateT, evalStateT, MonadState(put, get))
import Control.Monad.IO.Class (MonadIO)

import qualified T3.Game.BoardManagerImpl as BoardManager (isOpenLoc, getResult)
import T3.Core (Board, emptyBoard, boardList)
import T3.Game.Classes (Control(..), HasBoard(..), BoardManager(..))

import qualified T3.GameConsole.ControlImpl as Control
import qualified T3.GameConsole.BoardManagerImpl as BoardManager (insertAtLoc)
import qualified T3.GameConsole.ConsoleImpl as Console
import T3.GameConsole.Classes (Console(..))

newtype GameConsole a = GameConsole { unGameConsole :: StateT Board IO a }
  deriving (Functor, Applicative, Monad, MonadState Board, MonadIO)

io :: GameConsole a -> IO a
io (GameConsole m) = evalStateT m emptyBoard

instance HasBoard GameConsole where
  putBoard = put
  getBoard = get

instance BoardManager GameConsole where
  isOpenLoc = BoardManager.isOpenLoc
  insertAtLoc = BoardManager.insertAtLoc
  getResult = BoardManager.getResult

instance Console GameConsole where
  putStrLn = Console.putStrLn
  getLine = Console.getLine

instance Control GameConsole where
  move = Control.move
  forfeit = Control.forfeit
  tie = Control.tie
  end = Control.end
