module Handler.Start where

import Import
import T3.Server
import T3.Server.Lobby

postStartR :: Handler Value
postStartR = do
  startReq <- requireJsonBody
  resp <- liftIO newEmptyMVar
  srv <- appServer <$> getYesod
  authenticated <- liftIO . atomically $ authenticate srv (sreqUserCreds startReq)
  if not authenticated
    then returnJson (Nothing :: Maybe ())
    else do
      added <- liftIO $ addUserToLobby
        (srvLobby srv)
        (ucUserName $ sreqUserCreds startReq)
        (\matchInfo users step -> putMVar resp $ StartResponse matchInfo users (toGameState step))
      if added
        then do
          sresp <- liftIO $ readMVar resp
          returnJson sresp
        else returnJson (Nothing :: Maybe ())
