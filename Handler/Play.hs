module Handler.Play where

import qualified Data.Map as M
import Import
import T3.Server
import T3.Match

postPlayR :: MatchId -> MatchToken -> Handler Value
postPlayR matchId matchToken = do
  playReq <- requireJsonBody
  srv <- appServer <$> getYesod
  mUserCfg <- liftIO . atomically $ do
    let creds = preqUserCreds playReq
    authenicated <- authenticate srv creds
    if not authenicated
      then return Nothing
      else do
        mMatchCfg <- M.lookup matchId <$> readTVar (srvMatches srv)
        return $ authorize (ucUserName creds) matchToken =<< mMatchCfg
  case mUserCfg of
    Nothing -> returnJson ([] :: [()])
    Just userCfg -> do
      resp <- liftIO newEmptyMVar
      liftIO $ (userCfgSendLoc userCfg) (preqLoc playReq, putMVar resp)
      step <- liftIO $ readMVar resp
      returnJson $ PlayResponse (GameState (stepBoard step) Nothing)
