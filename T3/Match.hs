module T3.Match
  ( module T3.Match.Types
  , runMatch
  , tally
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
  , matchLog :: Win XO -> Lose XO -> Board -> IO ()
  , matchBoard :: Board
  , matchMoves :: [(XO, Loc)]
  }

newtype Match a = Match { unMatch :: StateT MatchData IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MatchData)

type UserInit = (UserName, Callback, IO (Loc, Callback))

runMatch
  :: UserInit
  -> UserInit
  -> (Win UserName -> Lose UserName -> Board -> IO ())
  -> IO ()
runMatch (xUN, xCB, xReq) (oUN, oCB, oReq) logger = let
  req X = xReq
  req O = oReq
  cb X = xCB
  cb O = oCB
  un X = xUN
  un O = oUN
  b = emptyBoard
  matchDat = MatchData req (cb X) (cb O) (\w l -> logger (fmap un w) (fmap un l)) b []
  in evalStateT (unMatch $ run b) matchDat

sendGameState :: XO -> Match ()
sendGameState xo = do
  s <- get
  liftIO $ (respXO xo s) (Step (matchBoard s) Nothing)

recvMove :: XO -> Match Loc
recvMove xo = do
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

tally :: Win XO -> Lose XO -> Match ()
tally w l = do
  s <- get
  liftIO $ (matchLog s) w l (matchBoard s)

updateBoard :: Board -> Match ()
updateBoard b = do
  match <- get
  put $ match { matchBoard = b }

logMove :: XO -> Loc -> Match ()
logMove xo loc = do
  match <- get
  put $ match { matchMoves = matchMoves match ++ [(xo, loc)] }

respXO :: XO -> MatchData -> Callback
respXO X = matchRespX
respXO O = matchRespO

instance Game Match  where
  move xo = do
    sendGameState xo
    recvMove xo
  forfeit (Win w) (Lose l) = do
    sendFinal l LossByDQ
    sendFinal w WonByDQ
  end (Win w) (Lose l) = do
    sendFinal w Won
    sendFinal l Loss
  tie = do
    sendFinal X Tied
    sendFinal O Tied
  step b xo loc = do
    logMove xo loc
    updateBoard b
