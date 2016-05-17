module T3.Match
  ( runMatch
  , delay
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import Control.Monad.Conc.ClassTmp

import T3.Game
import T3.Match.Types
import T3.Match.Impl.MonadConc

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
