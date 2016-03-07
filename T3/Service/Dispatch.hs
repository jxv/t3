module T3.Service.Dispatch where

import Prelude
import Data.Text (Text)
import Control.Concurrent
import Control.Concurrent.Chan
import T3.Game
import T3.Comm.Types
import T3.Session

type GameId = Text
type GameToken = Text

data UserConfig = UserConfig
  { userCfgUserId :: UserId
  , userCfgGameToken :: GameToken
  , userCfgSendLoc :: (Loc, Callback) -> IO ()
  }

data SessionConfig = SessionConfig
  { sessCfgX :: UserConfig
  , sessCfgO :: UserConfig
  , sessCfgKillSelf :: IO ()
  }

forkSession
  :: (UserId, GameToken, Callback)
  -> (UserId, GameToken, Callback)
  -> (Win UserId -> Lose UserId -> Board -> IO ())
  -> IO ()
  -> IO SessionConfig
forkSession (xUI, xGT, xCB) (oUI, oGT, oCB) logger end = do
  xChan <- newChan
  oChan <- newChan
  let x = (xUI, xCB, readChan xChan)
  let o = (oUI, oCB, readChan oChan)
  thid <- forkIO $ runSession x o logger
  return $ SessionConfig
    (UserConfig xUI xGT (writeChan xChan))
    (UserConfig oUI oGT (writeChan oChan))
    (killThread thid >> end)
