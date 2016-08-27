module T3.GameConsole.System
  ( System
  , io
  ) where

import Prelude hiding (putStrLn, getLine)
import Control.Monad.State (StateT, evalStateT, MonadState(put, get))
import Control.Monad.IO.Class (MonadIO)

import qualified T3.Game.BoardManagerImpl as BoardManager (isOpenLoc, getResult)
import T3.Core (Board, emptyBoard, boardList)
import T3.Game.Parts (Control(..), HasBoard(..), BoardManager(..))

import qualified T3.GameConsole.ControlImpl as Control
import qualified T3.GameConsole.BoardManagerImpl as BoardManager (insertAtLoc)
import qualified T3.GameConsole.ConsoleImpl as Console
import T3.GameConsole.Parts (Console(..))

newtype System a = System { unSystem :: StateT Board IO a }
  deriving (Functor, Applicative, Monad, MonadState Board, MonadIO)

io :: System a -> IO a
io system = evalStateT (unSystem system) emptyBoard

instance HasBoard System where
  putBoard = put
  getBoard = get

instance BoardManager System where
  isOpenLoc = BoardManager.isOpenLoc
  insertAtLoc = BoardManager.insertAtLoc
  getResult = BoardManager.getResult

instance Console System where
  putStrLn = Console.putStrLn
  getLine = Console.getLine

instance Control System where
  move = Control.move
  forfeit = Control.forfeit
  tie = Control.tie
  end = Control.end
