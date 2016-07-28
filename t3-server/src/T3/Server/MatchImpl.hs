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

class Monad m => HasBoard m where
  getBoard :: m Board
  putBoard :: Board -> m ()

class Monad m => HasActions m where
  getActions :: m [Action]
  appendAction :: Action -> m ()

class Monad m => HasConnection m where
  getConnection :: XO -> m Connection

class Monad m => Transmitter m where
  sendStep :: Connection -> Step -> m ()
  recvLoc :: Connection -> m Loc

class Monad m => MatchLogger m where
  logMatch :: [Action] -> Board -> Result -> m ()

class Monad m => Timeout m where
  timeout :: m a -> m () -> m a

sendGameState :: (HasBoard m, HasConnection m, Transmitter m) => XO -> m ()
sendGameState xo = do
  board <- getBoard
  conn <- getConnection xo
  sendStep conn (Step board Nothing)

recvAction :: (HasBoard m, HasConnection m, HasActions m, Transmitter m, MatchLogger m, Timeout m) => XO -> m Loc
recvAction xo = do
  conn <- getConnection xo
  timeout (recvLoc conn) (timeoutForfeit (Win $ yinYang xo) (Lose xo))

sendFinal :: (HasBoard m, HasConnection m, Transmitter m) => XO -> Final -> m ()
sendFinal xo final = do
  board <- getBoard
  conn <- getConnection xo
  sendStep conn (Step board (Just final))

tally :: (HasBoard m, HasActions m, MatchLogger m) => Result -> m ()
tally result = do
  actions <- getActions
  board <- getBoard
  logMatch actions board result

timeoutForfeit :: (HasBoard m, HasConnection m, HasActions m, Transmitter m, MatchLogger m) => Win XO -> Lose XO -> m ()
timeoutForfeit (Win w) (Lose l) = do
  tally (Winner w)
  sendFinal w WonByDQ
  sendFinal l LossByDQ

updateBoard :: HasBoard m => Board -> m ()
updateBoard board = putBoard board

logAction :: HasActions m => XO -> Loc -> m ()
logAction xo loc = appendAction (Action xo loc)
