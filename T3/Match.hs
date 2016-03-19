module T3.Match
  ( module T3.Match.Types
  , runMatch
  , UserInit
  , Callback
  , StartCallback
  ) where

import Prelude
import T3.Match.Types
import T3.Game

import Control.Monad.State.Strict

type Callback = Step -> IO ()
type StartCallback = MatchInfo -> Users -> Step -> IO ()

data MatchData = MatchData
  { matchReq :: XO -> IO (Loc, Callback)
  , matchRespX :: Callback
  , matchRespO :: Callback
  , matchLog :: [Action] -> Board -> Result -> IO ()
  , matchBoard :: Board
  , matchActions :: [Action]
  }

newtype Match a = Match { unMatch :: StateT MatchData IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MatchData)

type UserInit = (Callback, IO (Loc, Callback))

runMatch :: UserInit -> UserInit -> ([Action] -> Board -> Result -> IO ()) -> IO () -> IO ()
runMatch (xCB, xReq) (oCB, oReq) logger done = let
  req X = xReq
  req O = oReq
  cb X = xCB
  cb O = oCB
  b = emptyBoard
  matchDat = MatchData req (cb X) (cb O) logger b []
  in evalStateT (unMatch $ run b) matchDat >> done

sendGameState :: XO -> Match ()
sendGameState xo = do
  s <- get
  liftIO $ (respXO xo s) (Step (matchBoard s) Nothing)

recvAction :: XO -> Match Loc
recvAction xo = do
  req <- gets (flip matchReq xo)
  (loc, resp) <- liftIO req
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
  liftIO $ (respXO xo s) (Step (matchBoard s) (Just final))

tally :: Result -> Match ()
tally res = do
  s <- get
  liftIO $ matchLog s (matchActions s) (matchBoard s) res

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
  forfeit (Win w) (Lose l) = do
    tally (Winner w)
    sendFinal l LossByDQ
    sendFinal w WonByDQ
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
