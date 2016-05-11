module T3.Server.Dispatch.Impl.MonadConc where

import Control.Monad.Conc.ClassTmp

import T3.Server.Dispatch.Types
import T3.Game
import T3.Match

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
