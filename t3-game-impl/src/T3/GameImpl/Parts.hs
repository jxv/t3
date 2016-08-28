module T3.GameImpl.Parts
  ( Communicator(..)
  , Transmitter(..)
  ) where

import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result, Action)
import T3.GameImpl.Types (Final, Step)

class Monad m => Communicator m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()

class Monad m => Transmitter m where
  sendStep :: XO -> Step -> m ()
  recvLoc :: XO -> m Loc
