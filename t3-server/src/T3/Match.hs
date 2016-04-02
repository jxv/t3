module T3.Match
  ( module T3.Match.Types
  , Seconds(..)
  , runMatch
  , UserInit
  , Callback
  , StartCallback
  , delay
  ) where

import qualified Control.Concurrent.Async as IO
import qualified Control.Concurrent as IO
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.State.Strict

-- TODO: uncomment import when dejafu updates
-- import Control.Monad.Conc.Class (MonadConc(..))

import T3.Game
import T3.Match.Types

-- TODO: remove when dejafu updates
class Monad m => MonadConc m where
  threadDelay :: Int -> m ()
  race :: m a -> m b -> m (Either a b)

-- TODO: remove when dejafu updates
instance MonadConc IO where
  threadDelay = IO.threadDelay
  race = IO.race

type Callback m = Step -> m ()
type StartCallback m = MatchInfo -> Users -> Step -> m ()

newtype Seconds = Seconds Int
  deriving (Num, Show, Eq, Ord, Enum)

data MatchData m = MatchData
  { _matchReq :: XO -> m (Loc, Callback m)
  , _matchRespX :: Callback m
  , _matchRespO :: Callback m
  , _matchLog :: [Action] -> Board -> Result -> m ()
  , _matchBoard :: Board
  , _matchActions :: [Action]
  , _matchTimeoutLimit :: Maybe Seconds
  }

newtype MatchT m a = MatchT { unMatchT :: StateT (MatchData m) (EitherT (m ()) m) a }
  deriving (Functor, Applicative, Monad, MonadState (MatchData m))

instance MonadTrans MatchT where
  lift ma = MatchT . StateT $ \md -> do
    a <- EitherT (fmap return ma)
    return (a, md)

type UserInit m = (Callback m, m (Loc, Callback m))

runMatch :: MonadConc m => Maybe Seconds -> UserInit m -> UserInit m -> ([Action] -> Board -> Result -> m ()) -> m () -> m ()
runMatch timeoutLimit (xCB, xReq) (oCB, oReq) logger done = do
  let req X = xReq
      req O = oReq
  let cb X = xCB
      cb O = oCB
  let b = emptyBoard
  let matchDat = MatchData req (cb X) (cb O) logger b [] timeoutLimit
  matchResult <- runEitherT (evalStateT (unMatchT $ run b) matchDat)
  either id (const $ return ()) matchResult
  done

sendGameState :: MonadConc m => XO -> MatchT m ()
sendGameState xo = do
  md <- get
  lift $ (respXO xo md) (Step (_matchBoard md) Nothing)

delay :: MonadConc m => Seconds -> m ()
delay (Seconds n) = threadDelay (n * 1000000)

recvAction :: MonadConc m => XO -> MatchT m Loc
recvAction xo = do
  md <- get
  let req = _matchReq md xo
  let timeoutResponse = forfeitIO md (Win $ yinYang xo) (Lose xo)
  timeoutOrLoc <- lift $ do
    maybe
      (fmap Right req)
      (\secs -> race (delay secs >> return timeoutResponse) req)
      (_matchTimeoutLimit md)
  case timeoutOrLoc of
    Left timeout -> MatchT $ lift (left timeout)
    Right (loc, resp) -> do
      updateResp resp
      return loc
  where
    updateResp resp = do
      md <- get
      put $ case xo of
        X -> md{ _matchRespX = resp }
        O -> md{ _matchRespO = resp }

sendFinal :: MonadConc m => XO -> Final -> MatchT m ()
sendFinal xo f = do
  md <- get
  lift $ sendFinalIO md xo f

sendFinalIO :: MonadConc m => MatchData m -> XO -> Final -> m ()
sendFinalIO md xo f = (respXO xo md) (Step (_matchBoard md) (Just f))

tally :: MonadConc m => Result -> MatchT m ()
tally res = do
  md <- get
  lift $ tallyIO md res

tallyIO :: MonadConc m => MatchData m -> Result -> m ()
tallyIO md res = _matchLog md (_matchActions md) (_matchBoard md) res

forfeitIO :: MonadConc m => MatchData m -> Win XO -> Lose XO -> m ()
forfeitIO s (Win w) (Lose l) = do
  tallyIO s (Winner w)
  sendFinalIO s w WonByDQ
  sendFinalIO s l LossByDQ

updateBoard :: MonadConc m => Board -> MatchT m ()
updateBoard b = do
  md <- get
  put $ md{ _matchBoard = b }

logAction :: MonadConc m => XO -> Loc -> MatchT m ()
logAction xo loc = do
  md <- get
  put md{ _matchActions = _matchActions md ++ [Action xo loc] }

respXO :: MonadConc m => XO -> MatchData m -> Callback m
respXO X = _matchRespX
respXO O = _matchRespO

instance MonadConc m => Game (MatchT m) where
  move xo = do
    sendGameState xo
    recvAction xo
  forfeit w l = do
    md <- get
    lift $ forfeitIO md w l
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
