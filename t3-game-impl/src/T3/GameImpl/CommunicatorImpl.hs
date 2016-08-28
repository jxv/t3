module T3.GameImpl.CommunicatorImpl
  ( sendGameState
  , recvAction
  , sendFinal
  , updateBoard
  ) where

import T3.Core (XO, Loc, Result(Winner), Action(Action), Board, yinYang)
import T3.Game.Types (Win(Win), Lose(Lose))
import T3.Game.Parts (HasBoard(getBoard, putBoard))

import T3.GameImpl.Types (Final, Step(..))
import T3.GameImpl.Parts (Transmitter(sendStep, recvLoc))

sendGameState :: (HasBoard m, Transmitter m) => XO -> m ()
sendGameState xo = do
  board <- getBoard
  sendStep xo Step{ _stepBoard = board, _stepFinal = Nothing }

recvAction :: Transmitter m => XO -> m Loc
recvAction xo = recvLoc xo

sendFinal :: (HasBoard m, Transmitter m) => XO -> Final -> m ()
sendFinal xo final = do
  board <- getBoard
  sendStep xo Step{ _stepBoard = board, _stepFinal = Just final }

updateBoard :: HasBoard m => Board -> m ()
updateBoard board = putBoard board
