module T3.Match.GameCommImpl
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
import T3.Match.HasMatchState (HasMatchState(..))
import T3.Match.MatchTransmitter (MatchTransmitter(..))
import T3.Match.MatchLogger (MatchLogger(..))
import T3.Match.OnTimeout (OnTimeout(..))
import T3.Match.Stoppable (Stoppable(..))
import T3.Match.Milliseconds (Milliseconds)
import T3.Match.HasTimeoutLimit (HasTimeoutLimit(..))

sendGameState :: (HasMatchState m, MatchTransmitter m) => XO -> m ()
sendGameState xo = do
  board <- getBoard
  sendStep xo (Step board Nothing)

recvAction :: (HasMatchState m, MatchTransmitter m, MatchLogger m, OnTimeout m, HasTimeoutLimit m, Stoppable m) => XO -> m Loc
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

sendFinal :: (HasMatchState m, MatchTransmitter m) => XO -> Final -> m ()
sendFinal xo final = do
  board <- getBoard
  sendStep xo (Step board (Just final))

tally :: (HasMatchState m, MatchLogger m) => Result -> m ()
tally result = do
  actions <- getActions
  board <- getBoard
  logMatch actions board result

timeoutForfeit :: (HasMatchState m, MatchTransmitter m, MatchLogger m) => Win XO -> Lose XO -> m ()
timeoutForfeit (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w WonByDQ
  sendFinal l LossByDQ

updateBoard :: HasMatchState m => Board -> m ()
updateBoard board = putBoard board

logAction :: HasMatchState m => XO -> Loc -> m ()
logAction xo loc = appendAction (Action xo loc)
