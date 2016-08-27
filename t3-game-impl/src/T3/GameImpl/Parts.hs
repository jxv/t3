module T3.GameImpl.Parts
  ( Communicator(..)
  , Console(..)
  , HasActions(..)
  , Transmitter(..)
  , Finalizer(..)
  ) where

import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result, Action)
import T3.GameImpl.Types (Final, Step)
import T3.GameImpl.Milliseconds (Milliseconds)

class Monad m => Communicator m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Result -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()

class Monad m => Console m where
  printStdout :: Text -> m ()

class Monad m => HasActions m where
  getActions :: m [Action]
  putActions :: [Action] -> m ()

class Monad m => Transmitter m where
  sendStep :: XO -> Step -> m ()
  recvLoc :: XO -> m Loc

class Monad m => Finalizer m where
  finalize :: [Action] -> Board -> Result -> m ()
