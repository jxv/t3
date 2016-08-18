module T3.Server
  (
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Chan (newChan, Chan, writeChan, readChan)

import T3.Core (Loc, XO(..), Action, Board, Result)
import T3.Match
import T3.Match.Milliseconds (Milliseconds(..), Delayer(..))
import T3.Match.Types (Step)

import T3.Server.Lobby ()
import T3.Server.Matches ()

newtype ClientId = ClientId String
  deriving (Show, Eq)

newtype MatchId = MatchId String
  deriving (Show, Eq)

class Monad m => GetClientPair m where
  getClientPair :: m (ClientId, ClientId)

class Monad m => GenMatchId m where
  genMatchId :: m MatchId

class Monad m => ForkMatch m where
  forkMatch :: ([Action] -> Board -> Result -> IO ()) -> Maybe Milliseconds -> m (ThreadId, XO -> MatchCallbacks)

class Monad m => GetLogger m where
  getLogger :: m (MatchId -> ClientId -> ClientId -> [Action] -> Board -> Result -> IO ())

class Monad m => GetTimeoutLimit m where
  getTimeoutLimit :: m (Maybe Milliseconds)

class Monad m => InsertMatch m where
  insertMatch :: MatchId -> ThreadId -> (XO -> ClientId) -> (ClientId -> MatchCallbacks) -> m ()

data MatchCallbacks = MatchCallbacks
  { _matchCallbacksRecv :: IO Step
  , _matchCallbacksSend :: Loc -> IO ()
  }

dispatcher :: (GetClientPair m, ForkMatch m, GenMatchId m, GetLogger m, GetTimeoutLimit m, InsertMatch m) => m ()
dispatcher = dispatchMatch >> dispatcher

dispatchMatch :: (GetClientPair m, ForkMatch m, GenMatchId m, GetLogger m, GetTimeoutLimit m, InsertMatch m) => m ()
dispatchMatch = do
  (clientA, clientB) <- getClientPair
  matchId <- genMatchId
  logger <- getLogger
  timeoutLimit <- getTimeoutLimit
  (threadId, xoMatchCallbacks) <- forkMatch (logger matchId clientA clientB) timeoutLimit
  let clients = xoMap clientA clientB
  let matchCallbacks client = case client of clientA -> xoMatchCallbacks X; clientB -> xoMatchCallbacks O
  insertMatch matchId threadId clients matchCallbacks

xoMap :: a -> a -> XO -> a
xoMap x _ X = x
xoMap _ o O = o

class Monad m => LobbyAccess m where
  mayGetClientPair :: m (Maybe (ClientId, ClientId))

getClientPair' :: (Delayer m, LobbyAccess m) => m (ClientId, ClientId)
getClientPair' = do
  maybePair <- mayGetClientPair
  case maybePair of
    Just pair -> return pair
    Nothing -> do
      delay (Milliseconds 1000)
      getClientPair'

class Monad m => MakeCallbacks m where
  makeCallbacks :: m (Callbacks, MatchCallbacks)

makeCallbacks' :: IO (Callbacks, MatchCallbacks)
makeCallbacks' = do
  loc <- newChan
  step <- newChan
  return $
    ( Callbacks { _callbacksSend = writeChan step, _callbacksRecv = readChan loc }
    , MatchCallbacks { _matchCallbacksSend = writeChan loc, _matchCallbacksRecv = readChan step }
    )

class Monad m => ForkStartMatch m where
  forkStartMatch :: MatchEnv -> m ThreadId

forkStartMatch' :: MatchEnv -> IO ThreadId
forkStartMatch' = forkIO . startMatch

forkMatch' :: (MakeCallbacks m, ForkStartMatch m) => ([Action] -> Board -> Result -> IO ()) -> Maybe Milliseconds -> m (ThreadId, XO -> MatchCallbacks)
forkMatch' logger timeoutLimit = do
  cb <- xoMap <$> makeCallbacks <*> makeCallbacks
  let env = MatchEnv{ _callbacks = fmap fst cb, _timeoutLimit = timeoutLimit, _logger = logger }
  threadId <- forkStartMatch env
  return (threadId, fmap snd cb)

stdoutLogger :: MatchId -> ClientId -> ClientId -> [Action] -> Board -> Result -> IO ()
stdoutLogger matchId clientA clientB actions board result = do
  putStrLn $ show matchId
  putStrLn $ show (clientA, clientB)
  putStrLn $ show actions
  putStrLn $ show board
  putStrLn $ show result
