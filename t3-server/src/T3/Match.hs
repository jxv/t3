module T3.Match
  ( module T3.Match.Types
  , Seconds(..)
  , runMatch
  , UserInit
  , Callback
  , StartCallback
  ) where

import Prelude
import T3.Match.Types
import T3.Game
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Either

import Control.Monad.State.Strict

type Callback = Step -> IO ()
type StartCallback = MatchInfo -> Users -> Step -> IO ()

newtype Seconds = Seconds Int
  deriving (Num, Show, Eq, Ord, Enum)

data MatchData = MatchData
  { matchReq :: XO -> IO (Loc, Callback)
  , matchRespX :: Callback
  , matchRespO :: Callback
  , matchLog :: [Action] -> Board -> Result -> IO ()
  , matchBoard :: Board
  , matchActions :: [Action]
  , matchTimeoutLimit :: Maybe Seconds
  }

newtype Match a = Match { unMatch :: EitherT (IO ()) (StateT MatchData IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MatchData)

type UserInit = (Callback, IO (Loc, Callback))

runMatch :: Maybe Seconds -> UserInit -> UserInit -> ([Action] -> Board -> Result -> IO ()) -> IO () -> IO ()
runMatch timeoutLimit (xCB, xReq) (oCB, oReq) logger done = do
  let req X = xReq
      req O = oReq
  let cb X = xCB
      cb O = oCB
  let b = emptyBoard
  let matchDat = MatchData req (cb X) (cb O) logger b [] timeoutLimit
  matchResult <- evalStateT (runEitherT $ unMatch $ run b) matchDat
  either id (const $ return ()) matchResult
  done

sendGameState :: XO -> Match ()
sendGameState xo = do
  s <- get
  liftIO $ (respXO xo s) (Step (matchBoard s) Nothing)

delay :: Seconds -> IO ()
delay (Seconds n) = threadDelay (n * 1000000)

recvAction :: XO -> Match Loc
recvAction xo = do
  req <- gets (flip matchReq xo)
  s <- get
  let timeoutResponse = forfeitIO s (Win $ yinYang xo) (Lose xo)
  timeoutOrLoc <- liftIO $
    maybe
      (fmap Right req)
      (\secs -> race (delay secs >> return timeoutResponse) req)
      (matchTimeoutLimit s)
  case timeoutOrLoc of
    Left timeout -> Match (left timeout)
    Right (loc, resp) -> do
      updateResp resp
      return loc
  where
    updateResp resp = do
      match <- get
      put $ case xo of
        X -> match { matchRespX = resp }
        O -> match { matchRespO = resp }

sendFinal :: XO -> Final -> Match ()
sendFinal xo final = do
  s <- get
  liftIO $ sendFinalIO s xo final

sendFinalIO :: MatchData -> XO -> Final -> IO ()
sendFinalIO s xo final = liftIO $ (respXO xo s) (Step (matchBoard s) (Just final))

tally :: Result -> Match ()
tally res = do
  s <- get
  liftIO $ matchLog s (matchActions s) (matchBoard s) res

tallyIO :: MatchData -> Result -> IO ()
tallyIO s res =  matchLog s (matchActions s) (matchBoard s) res

forfeitIO :: MatchData -> Win XO -> Lose XO -> IO ()
forfeitIO s (Win w) (Lose l) = do
  tallyIO s (Winner w)
  sendFinalIO s w WonByDQ
  sendFinalIO s l LossByDQ

updateBoard :: Board -> Match ()
updateBoard b = do
  match <- get
  put $ match { matchBoard = b }

logAction :: XO -> Loc -> Match ()
logAction xo loc = do
  match <- get
  put $ match { matchActions = matchActions match ++ [Action xo loc] }

respXO :: XO -> MatchData -> Callback
respXO X = matchRespX
respXO O = matchRespO

instance Game Match  where
  move xo = do
    sendGameState xo
    recvAction xo
  forfeit w l = do
    s <- get
    liftIO $ forfeitIO s w l
  end (Win w) (Lose l) = do
    tally (Winner w)
    sendFinal w Won
    sendFinal l Loss
  tie = do
    tally Tie
    sendFinal X Tied
    sendFinal O Tied
  step b xo loc = do
    logAction xo loc
    updateBoard b
