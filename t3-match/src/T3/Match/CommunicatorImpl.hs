module T3.Match.CommunicatorImpl
  ( sendGameState
  , recvAction
  , sendFinal
  , tally
  , updateBoard
  , logAction
  ) where

import T3.Core (XO(..), Loc(..), Result(..), Action(..), Board, yinYang)
import T3.Game (Win(..), Lose(..))
import T3.Match.Types (Final(..), Step(..))
import T3.Match.HasState (HasState(..))
import T3.Match.Transmitter (Transmitter(..))
import T3.Match.Logger (Logger(..))
import T3.Match.OnTimeout (OnTimeout(..))
import T3.Match.Stoppable (Stoppable(..))
import T3.Match.Milliseconds (Milliseconds)
import T3.Match.HasTimeoutLimit (HasTimeoutLimit(..))

sendGameState :: (HasState m, Transmitter m) => XO -> m ()
sendGameState xo = do
  board <- getBoard
  sendStep xo (Step board Nothing)

recvAction :: (HasState m, Transmitter m, Logger m, OnTimeout m, HasTimeoutLimit m, Stoppable m) => XO -> m Loc
recvAction xo = do
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

sendFinal :: (HasState m, Transmitter m) => XO -> Final -> m ()
sendFinal xo final = do
  board <- getBoard
  sendStep xo (Step board (Just final))

tally :: (HasState m, Logger m) => Result -> m ()
tally result = do
  actions <- getActions
  board <- getBoard
  logIt actions board result

timeoutForfeit :: (HasState m, Transmitter m, Logger m) => Win XO -> Lose XO -> m ()
timeoutForfeit (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w WonByDQ
  sendFinal l LossByDQ

updateBoard :: HasState m => Board -> m ()
updateBoard board = putBoard board

logAction :: HasState m => XO -> Loc -> m ()
logAction xo loc = appendAction (Action xo loc)

appendAction :: HasState m => Action -> m ()
appendAction action = do
  actions <- getActions
  putActions $ actions ++ [action]
