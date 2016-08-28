module T3.GameImpl.Parts
  ( Communicator(..)
  , Console(..)
  , Transmitter(..)
  ) where

import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result, Action)
import T3.GameImpl.Types (Final, Step)

class Monad m => Communicator m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()

class Monad m => Console m where
  printStdout :: Text -> m ()

class Monad m => Transmitter m where
  sendStep :: XO -> Step -> m ()
  recvLoc :: XO -> m Loc
