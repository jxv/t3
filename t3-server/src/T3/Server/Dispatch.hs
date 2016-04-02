module T3.Server.Dispatch where

import Prelude
import Control.Concurrent
import T3.Game
import T3.Match

data UserConfig = UserConfig
  { userCfgUserName :: UserName
  , userCfgMatchToken :: MatchToken
  , userCfgSendLoc :: (Loc, Callback IO) -> IO ()
  }

data MatchConfig = MatchConfig
  { matchCfgX :: UserConfig
  , matchCfgO :: UserConfig
  , matchCfgDie :: IO ()
  }

forkMatch
  :: Maybe Seconds
  -> (UserName, MatchToken, Callback IO)
  -> (UserName, MatchToken, Callback IO)
  -> ([Action] -> Board -> Result -> IO ())
  -> IO ()
  -> IO MatchConfig
forkMatch timeoutLimit (xUI, xGT, xCB) (oUI, oGT, oCB) logger done = do
  xChan <- newChan
  oChan <- newChan
  let x = (xCB, readChan xChan)
  let o = (oCB, readChan oChan)
  thid <- forkIO $ runMatch timeoutLimit x o logger done
  return $ MatchConfig
    (UserConfig xUI xGT (writeChan xChan))
    (UserConfig oUI oGT (writeChan oChan))
    (killThread thid >> done)
