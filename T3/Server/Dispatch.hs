module T3.Server.Dispatch where

import Prelude
import Control.Concurrent
import T3.Game
import T3.Match

data UserConfig = UserConfig
  { userCfgUserId :: UserId
  , userCfgMatchToken :: MatchToken
  , userCfgSendLoc :: (Loc, Callback) -> IO ()
  }

data MatchConfig = MatchConfig
  { matchCfgX :: UserConfig
  , matchCfgO :: UserConfig
  , matchCfgDie :: IO ()
  }

forkMatch
  :: (UserId, MatchToken, Callback)
  -> (UserId, MatchToken, Callback)
  -> (Win UserId -> Lose UserId -> Board -> IO ())
  -> IO ()
  -> IO MatchConfig
forkMatch (xUI, xGT, xCB) (oUI, oGT, oCB) logger done = do
  xChan <- newChan
  oChan <- newChan
  let x = (xUI, xCB, readChan xChan)
  let o = (oUI, oCB, readChan oChan)
  thid <- forkIO $ runMatch x o logger
  return $ MatchConfig
    (UserConfig xUI xGT (writeChan xChan))
    (UserConfig oUI oGT (writeChan oChan))
    (killThread thid >> done)
