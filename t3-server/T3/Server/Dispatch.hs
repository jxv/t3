module T3.Server.Dispatch where

import Prelude
import Control.Concurrent
import T3.Game
import T3.Match

data UserConfig = UserConfig
  { userCfgUserName :: UserName
  , userCfgMatchToken :: MatchToken
  , userCfgSendLoc :: (Loc, Callback) -> IO ()
  }

data MatchConfig = MatchConfig
  { matchCfgX :: UserConfig
  , matchCfgO :: UserConfig
  , matchCfgDie :: IO ()
  }

forkMatch
  :: (UserName, MatchToken, Callback)
  -> (UserName, MatchToken, Callback)
  -> ([Action] -> Board -> Result -> IO ())
  -> IO ()
  -> IO MatchConfig
forkMatch (xUI, xGT, xCB) (oUI, oGT, oCB) logger done = do
  xChan <- newChan
  oChan <- newChan
  let x = (xCB, readChan xChan)
  let o = (oCB, readChan oChan)
  thid <- forkIO $ runMatch x o logger done
  return $ MatchConfig
    (UserConfig xUI xGT (writeChan xChan))
    (UserConfig oUI oGT (writeChan oChan))
    (killThread thid >> done)
