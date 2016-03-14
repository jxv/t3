module Handler.Play where

import qualified Data.Map as M
import Import
import T3.Server
import T3.Match

postPlayR :: MatchId -> MatchToken -> Handler Value
postPlayR matchId matchToken = do
  playReq <- requireJsonBody
  resp <- liftIO newEmptyMVar
  srv <- appServer <$> getYesod
  mUserCfg <- liftIO . atomically $ do
    let creds = preqUserCreds playReq
    authenicated <- authenticate srv creds
    case authenicated of
      False -> return Nothing
      True -> do
        mMatchCfg <- M.lookup matchId <$> readTVar (srvMatches srv)
        return $ authorize (ucUserName creds) matchToken =<< mMatchCfg
  case mUserCfg of
    Nothing -> returnJson ([] :: [Int])
    Just userCfg -> do
      liftIO $ play userCfg (preqLoc playReq)
      board <- liftIO $ readMVar resp
      returnJson $ PlayResponse (GameState board Nothing)
