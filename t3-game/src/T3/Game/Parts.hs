module T3.Game.Parts
  ( BoardManager(..)
  , HasBoard(..)
  , Control(..)
  , Communicator(..)
  , Transmitter(..)
  , Play(..)
  ) where

import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result)
import T3.Game.Types

class Monad m => Play m where
  play :: XO -> XO -> m ()

class Monad m => BoardManager m where
  isOpenLoc :: Loc -> m Bool
  insertAtLoc :: Loc -> XO -> m ()
  getResult :: m Result

class Monad m => HasBoard m where
  getBoard :: m Board
  putBoard :: Board -> m ()

class Monad m => Control m where
  move :: XO -> m Loc
  forfeit :: Win XO -> Lose XO -> m ()
  end :: Win XO -> Lose XO -> m ()
  tie :: m ()

class Monad m => Communicator m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()

class Monad m => Transmitter m where
  sendStep :: XO -> Step -> m ()
  recvLoc :: XO -> m Loc
