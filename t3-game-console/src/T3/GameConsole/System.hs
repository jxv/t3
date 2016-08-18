module T3.GameConsole.System
  ( System
  , io
  ) where


import Prelude hiding (putStrLn, getLine)
import Control.Monad.State (StateT, evalStateT, MonadState(put, get))
import Control.Monad.IO.Class (MonadIO)

import qualified T3.Game.BoardManagerImpl as BoardManager (isOpenLoc, getResult)
import T3.Core (Board, emptyBoard, boardList)
import T3.Game.Game (Game(..))
import T3.Game.HasBoard (HasBoard(..))
import T3.Game.BoardManager (BoardManager(..))

import qualified T3.GameConsole.GameImpl as Game
import qualified T3.GameConsole.BoardManagerImpl as BoardManager (insertAtLoc)
import qualified T3.GameConsole.ConsoleImpl as Console
import T3.GameConsole.Console (Console(..))

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

instance Game System where
  move = Game.move
  forfeit = Game.forfeit
  tie = Game.tie
  end = Game.end
