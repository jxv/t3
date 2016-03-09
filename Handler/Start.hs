module Handler.Start where

import Import
import T3.Web
import T3.Server
import T3.Server.Lobby
import T3.Web.Instance ()

postStartR :: Handler Value
postStartR = do
  userKey <- liftIO $ genBase64 32
  resp <- liftIO $ newEmptyMVar
  srv <- appServer <$> getYesod
  liftIO $ addUserToLobby (srvLobby srv) userKey (\matchId matchToken board -> putMVar resp (matchId, matchToken, board))
  (matchId, matchToken, board) <- liftIO $ readMVar resp
  returnJson $ [show userKey, show matchId, show matchToken, show board]
