module T3.Server.MatchImpl
  ( sendGameState
  , recvAction
  , sendFinal
  , tally
  , updateBoard
  , logAction
  ) where

import T3.Core (XO(..), Loc(..), Result(..), Action(..), Board, yinYang)
import T3.Game (Win(..), Lose(..))
import T3.Server (Seconds(..), Callback, Final(..), Step(..))
import T3.Server.Connection (Connection)
import T3.Server.HasMatchState (HasMatchState(..))
import T3.Server.HasConnection (HasConnection(..))
import T3.Server.MatchTransmitter (MatchTransmitter(..))
import T3.Server.MatchLogger (MatchLogger(..))
import T3.Server.OnTimeout (OnTimeout(..))

sendGameState :: (HasMatchState m, HasConnection m, MatchTransmitter m) => XO -> m ()
sendGameState xo = do
  board <- getBoard
  conn <- getConnection xo
  sendStep conn (Step board Nothing)

recvAction :: (HasMatchState m, HasConnection m, MatchTransmitter m, MatchLogger m, OnTimeout m) => XO -> m Loc
recvAction xo = do
  conn <- getConnection xo
  onTimeout (recvLoc conn) (timeoutForfeit (Win $ yinYang xo) (Lose xo))

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
