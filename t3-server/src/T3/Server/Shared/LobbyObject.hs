module T3.Server.Shared.LobbyObject
  ( newLobbyObject'
  ) where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import T3.Server.Gen (genUserId', genHashCode')
import T3.Server.Types

newLobbyObject' :: MonadIO m => m LobbyObject
newLobbyObject' = do
  hc <- genHashCode'
  p <- liftIO $ newTVarIO []
  w <- liftIO $ newTVarIO Map.empty
  return LobbyObject
    { _lobbyObjectHashCode = hc
    , _lobbyObjectTransferUser = transferUser p
    , _lobbyObjectDequeueUser = dequeueUser p w
    , _lobbyObjectAnnounceGame = announceGame w
    }

type UserQueue = [(UserId, MVar GameId)]
type AnnounceDeck = Map.Map GameId [MVar GameId]

transferUser :: TVar UserQueue -> UserId -> IO (Maybe GameId)
transferUser p i = do
  ref <- newEmptyMVar
  atomically $ modifyTVar p $ \q -> q ++ [(i, ref)]
  gameId <- takeMVar ref
  return $ Just gameId

dequeueUser :: TVar UserQueue -> TVar AnnounceDeck -> GameId -> IO (Maybe UserId)
dequeueUser p w i = do
  maybePair <- atomically $ do
    q <- readTVar p
    case q of
      [] -> do
        return Nothing
      (pair:q') -> do
        writeTVar p q'
        return $ Just pair

  case maybePair of
    Nothing -> return Nothing
    Just (userId, ref) -> do
      atomically $ do
        m <- readTVar w
        let m' = appendAt m i ref
        writeTVar w m'
      return $ Just userId

appendAt :: Ord k => Map.Map k [a] -> k -> a -> Map.Map k [a]
appendAt m k a = Map.alter (\case Nothing -> Just [a]; Just as -> Just (a:as)) k m

announceGame :: TVar AnnounceDeck -> GameId -> IO ()
announceGame w i = do
  refs <- atomically $ do
    m <- readTVar w
    case Map.lookup i m of
      Nothing -> do
        return []
      Just refs -> do
        let m' = Map.delete i m
        writeTVar w m'
        return refs
  mapM_ (\ref -> putMVar ref i) refs
