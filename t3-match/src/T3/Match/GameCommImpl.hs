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
import T3.Match.Types (Seconds(..), Callback, Final(..), Step(..))
import T3.Match.Connection (Connection)
import T3.Match.HasMatchState (HasMatchState(..))
import T3.Match.HasConnection (HasConnection(..))
import T3.Match.MatchTransmitter (MatchTransmitter(..))
import T3.Match.MatchLogger (MatchLogger(..))
import T3.Match.OnTimeout (OnTimeout(..))
import T3.Match.Stoppable (Stoppable(..))
import T3.Match.Milliseconds (Milliseconds)
import T3.Match.HasTimeoutLimit (HasTimeoutLimit(..))

sendGameState :: (HasMatchState m, HasConnection m, MatchTransmitter m) => XO -> m ()
sendGameState xo = do
  board <- getBoard
  conn <- getConnection xo
  sendStep conn (Step board Nothing)

recvAction :: (HasMatchState m, HasConnection m, MatchTransmitter m, MatchLogger m, OnTimeout m, HasTimeoutLimit m, Stoppable m) => XO -> m Loc
recvAction xo = do
  conn <- getConnection xo
  maybeTimeoutLimit <- getTimeoutLimit
  case maybeTimeoutLimit of
    Nothing -> recvLoc conn
    Just ms -> do
      maybeLoc <- onTimeout (recvLoc conn) ms
      case maybeLoc of
        Just loc -> return loc
        Nothing -> do
          timeoutForfeit (Win $ yinYang xo) (Lose xo)
          stop

sendFinal :: (HasMatchState m, HasConnection m, MatchTransmitter m) => XO -> Final -> m ()
sendFinal xo final = do
  board <- getBoard
  conn <- getConnection xo
  sendStep conn (Step board (Just final))

tally :: (HasMatchState m, MatchLogger m) => Result -> m ()
tally result = do
  actions <- getActions
  board <- getBoard
  logMatch actions board result

timeoutForfeit :: (HasMatchState m, HasConnection m, MatchTransmitter m, MatchLogger m) => Win XO -> Lose XO -> m ()
timeoutForfeit (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w WonByDQ
  sendFinal l LossByDQ

updateBoard :: HasMatchState m => Board -> m ()
updateBoard board = putBoard board

logAction :: HasMatchState m => XO -> Loc -> m ()
logAction xo loc = appendAction (Action xo loc)
