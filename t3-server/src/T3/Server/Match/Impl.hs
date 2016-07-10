module T3.Server.Match.Impl
  ( MatchT(..)
  , sendGameState
  , recvAction
  , sendFinal
  , tally
  , updateBoard
  , logAction
  , delay
  ) where

import Control.Concurrent.Async
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import Control.Monad.Conc.Class

import T3.Core (XO(..), Loc(..), Result(..), Action(..), Board, yinYang)
import T3.Game
import T3.Server.Match hiding (Match(..)) -- types

sendGameState :: MonadConc m => XO -> MatchT m ()
sendGameState xo = do
  md <- get
  lift $ (respXO xo md) (Step (_matchBoard md) Nothing)

recvAction :: MonadConc m => XO -> MatchT m Loc
recvAction xo = do
  md <- get
  let req = _matchReq md xo
  let timeoutResponse = forfeit' md (Win $ yinYang xo) (Lose xo)
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
  lift $ sendFinal' md xo f

sendFinal' :: MonadConc m => MatchData m -> XO -> Final -> m ()
sendFinal' md xo f = (respXO xo md) (Step (_matchBoard md) (Just f))

tally :: MonadConc m => Result -> MatchT m ()
tally res = do
  md <- get
  lift $ tally' md res

tally' :: MonadConc m => MatchData m -> Result -> m ()
tally' md res = _matchLog md (_matchActions md) (_matchBoard md) res

forfeit' :: MonadConc m => MatchData m -> Win XO -> Lose XO -> m ()
forfeit' s (Win w) (Lose l) = do
  tally' s (Winner w)
  sendFinal' s w WonByDQ
  sendFinal' s l LossByDQ

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

delay :: MonadConc m => Seconds -> m ()
delay (Seconds n) = threadDelay (n * 1000000)

newtype MatchT m a = MatchT { unMatchT :: StateT (MatchData m) (EitherT (m ()) m) a }
  deriving (Functor, Applicative, Monad, MonadState (MatchData m))

instance MonadTrans MatchT where
  lift ma = MatchT . StateT $ \md -> do
    a <- EitherT (fmap return ma)
    return (a, md)

instance MonadConc m => Game (MatchT m) where
  move xo = do
    sendGameState xo
    recvAction xo
  forfeit w l = do
    md <- get
    lift $ forfeit' md w l
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
