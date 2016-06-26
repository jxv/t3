module T3.Server.Dispatch.Impl.Conc
  ( forkMatch
  ) where

import Control.Concurrent.Classy.Chan
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import Control.Monad.Conc.Class

import T3.Core (XO(..), emptyBoard)
import T3.Game
import T3.Game.Run (run)
import T3.Match.Types
import T3.Match.Impl.Conc (MatchT(..), delay)
import T3.Server.Dispatch.Types

forkMatch
  :: MonadConc m
  => Maybe Seconds
  -> (UserName, MatchToken, Callback m)
  -> (UserName, MatchToken, Callback m)
  -> ([Action] -> Board -> Result -> m ())
  -> m ()
  -> m (MatchConfig m)
forkMatch timeoutLimit (xUI, xGT, xCB) (oUI, oGT, oCB) logger done = do
  xChan <- newChan
  oChan <- newChan
  let x = (xCB, readChan xChan)
  let o = (oCB, readChan oChan)
  thid <- fork $ runMatch timeoutLimit x o logger done
  return $ MatchConfig
    (UserConfig xUI xGT (writeChan xChan))
    (UserConfig oUI oGT (writeChan oChan))
    (killThread thid >> done)

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
