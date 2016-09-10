module T3.GameCallbacks.Communicator
  ( Communicator(..)
  , sendGameState'
  , recvAction'
  , sendFinal'
  , tally'
  , updateBoard'
  , logAction'
  ) where

import T3.Core (XO, Loc, Result(Winner), Action(Action), Board, yinYang)
import T3.Game.HasBoard (HasBoard(getBoard, putBoard))
import T3.Game.Types (Win(Win), Lose(Lose))
import T3.GameCallbacks.Types (Final(..), Step(..))
import T3.GameCallbacks.Milliseconds (Milliseconds)
import T3.GameCallbacks.HasActions (HasActions(putActions, getActions))
import T3.GameCallbacks.Transmitter (Transmitter(sendStep, recvLoc))
import T3.GameCallbacks.Finalizer (Finalizer(finalize))
import T3.GameCallbacks.OnTimeout (OnTimeout(onTimeout))
import T3.GameCallbacks.Stoppable (Stoppable(stop))
import T3.GameCallbacks.HasTimeoutLimit (HasTimeoutLimit(getTimeoutLimit))

class Monad m => Communicator m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Result -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()

sendGameState' :: (HasBoard m, Transmitter m) => XO -> m ()
sendGameState' xo = do
  board <- getBoard
  sendStep xo Step{ _stepBoard = board, _stepFinal = Nothing }

recvAction' :: (HasBoard m, HasActions m, Transmitter m, Finalizer m, OnTimeout m, HasTimeoutLimit m, Stoppable m) => XO -> m Loc
recvAction' xo = do
  maybeTimeoutLimit <- getTimeoutLimit
  case maybeTimeoutLimit of
    Nothing -> recvLoc xo
    Just ms -> do
      maybeLoc <- onTimeout (recvLoc xo) ms
      case maybeLoc of
        Just loc -> return loc
        Nothing -> do
          timeoutForfeit (Win $ yinYang xo) (Lose xo)
          stop

sendFinal' :: (HasBoard m, Transmitter m) => XO -> Final -> m ()
sendFinal' xo final = do
  board <- getBoard
  sendStep xo Step{ _stepBoard = board, _stepFinal = Just final }

tally' :: (HasBoard m, HasActions m, Finalizer m) => Result -> m ()
tally' result = do
  actions <- getActions
  board <- getBoard
  finalize actions board result

timeoutForfeit :: (HasBoard m, HasActions m, Transmitter m, Finalizer m) => Win XO -> Lose XO -> m ()
timeoutForfeit (Win w) (Lose l) = do
  tally' (Winner w)
  sendFinal' w WonByDQ
  sendFinal' l LossByDQ

updateBoard' :: HasBoard m => Board -> m ()
updateBoard' board = putBoard board

logAction' :: HasActions m => XO -> Loc -> m ()
logAction' xo loc = appendAction (Action xo loc)

appendAction :: HasActions m => Action -> m ()
appendAction action = do
  actions <- getActions
  putActions $ actions ++ [action]
