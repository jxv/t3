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
  liftIO $ addUserToLobby (srvLobby srv) userKey (\gameId gameToken board -> putMVar resp (gameId, gameToken, board))
  (gameId, gameToken, board) <- liftIO $ readMVar resp
  returnJson $ [show userKey, show gameId, show gameToken, show board]
