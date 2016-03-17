module Handler.Start where

import Import
import T3.Match
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
        (\matchId matchToken board -> putMVar resp (matchId, matchToken, board))
      if added
        then do
          (matchId, matchToken, step) <- liftIO $ readMVar resp
          returnJson $ StartResponse (MatchInfo matchId matchToken) (GameState (stepBoard step) (stepFinal step))
        else returnJson (Nothing :: Maybe ())
