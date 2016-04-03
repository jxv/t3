module T3.Server.Dispatch where

import Control.Monad.Conc.ClassTmp

import T3.Game
import T3.Match

data UserConfig m = UserConfig
  { _userCfgUserName :: UserName
  , _userCfgMatchToken :: MatchToken
  , _userCfgSendLoc :: (Loc, Callback m) -> m ()
  }

data MatchConfig m = MatchConfig
  { _matchCfgX :: UserConfig m
  , _matchCfgO :: UserConfig m
  , _matchCfgDie :: m ()
  }

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
