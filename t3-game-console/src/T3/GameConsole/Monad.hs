module T3.GameConsole.Monad
  ( GameConsole
  , runGameConsole
  ) where

import Prelude hiding (putStrLn, getLine)
import Control.Monad.State (StateT, evalStateT, MonadState(put, get))
import Control.Monad.IO.Class (MonadIO)

import T3.Core (Board)

import T3.Game.Monad (GameT, runGameT)

import T3.Game.Classes (BoardManager(..), Control(..), HasBoard, Play(..))
import T3.GameConsole.Classes (Console(..))

import qualified T3.Game.Play as Play

import qualified T3.GameConsole.BoardManager as BoardManager
import qualified T3.GameConsole.Control as Control
import qualified T3.GameConsole.Console as Console

newtype GameConsole a = GameConsole { unGameConsole :: GameT IO a }
  deriving (Functor, Applicative, Monad, HasBoard, MonadIO)

runGameConsole :: GameConsole a -> Board -> IO a
runGameConsole (GameConsole m) board = runGameT m board

instance Play GameConsole where
  play = Play.play'

instance BoardManager GameConsole where
  isOpenLoc = BoardManager.isOpenLoc
  insertAtLoc = BoardManager.insertAtLoc
  getResult = BoardManager.getResult

instance Control GameConsole where
  move = Control.move
  forfeit = Control.forfeit
  tie = Control.tie
  end = Control.end

instance Console GameConsole where
  putStrLn = Console.putStrLn
  getLine = Console.getLine
