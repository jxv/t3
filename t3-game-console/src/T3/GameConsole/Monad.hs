module T3.GameConsole.Monad
  ( GameConsole
  , runGameConsole
  ) where

import Prelude hiding (putStrLn, getLine)
import Control.Monad.State (StateT, evalStateT, MonadState(put, get))
import Control.Monad.IO.Class (MonadIO)

import T3.Core (Board)

import T3.Game.Play
import T3.Game.GameState (GameStateT, runGameStateT)
import T3.Game.HasBoard (HasBoard(..))
import T3.Game.BoardManager (BoardManager(..))
import T3.Game.Control (Control(..))

import T3.GameConsole.BoardManager
import T3.GameConsole.Control
import T3.GameConsole.Console

newtype GameConsole a = GameConsole { unGameConsole :: GameStateT IO a }
  deriving (Functor, Applicative, Monad, HasBoard, MonadIO)

runGameConsole :: GameConsole a -> Board -> IO a
runGameConsole (GameConsole m) board = runGameStateT m board

instance Play GameConsole where
  play = play'

instance BoardManager GameConsole where
  isOpenLoc = isOpenLoc'
  insertAtLoc = insertAtLoc'
  getResult = getResult'

instance Control GameConsole where
  move = move'
  forfeit = forfeit'
  tie = tie'
  end = end'

instance Console GameConsole where
  putStrLn = putStrLn'
  getLine = getLine'
