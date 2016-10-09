module T3.Server.SharedCb
  ( newRegistryCb'
  , newLobbyCb'
  , newGamesCb'
  , newResultsCb'
  , newGameCb'
  ) where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import T3.Server.Gen (genUserId', genHashCode')
import T3.Server.Types

type UserRec = (Name, Token)
type UserMap = Map.Map UserId UserRec

newRegistryCb' :: MonadIO m => m RegistryCb
newRegistryCb' = do
  hc <- genHashCode'
  w <- liftIO $ newTVarIO Map.empty
  return RegistryCb
    { _registryCbHashCode = hc
    , _registryCbInsertUser = insertUser w
    , _registryCbGetUserById = getUserById w
    }

insertUser :: TVar UserMap -> UserRec -> IO (Maybe UserId)
insertUser w rec = do
  userId <- genUserId'
  atomically $ do
    m <- readTVar w
    if Map.notMember userId m
      then do
        writeTVar w (Map.insert userId rec m)
        return (Just userId)
      else return Nothing

getUserById :: TVar UserMap -> UserId -> IO (Maybe UserRec)
getUserById w i = atomically $ do
  m <- readTVar w
  return $ Map.lookup i m

newLobbyCb' :: MonadIO m => m LobbyCb
newLobbyCb' = do
  hc <- genHashCode'
  p <- liftIO $ newTVarIO []
  w <- liftIO $ newTVarIO Map.empty
  return LobbyCb
    { _lobbyCbHashCode = hc
    , _lobbyCbTransferUser = transferUser p
    , _lobbyCbDequeueUser = dequeueUser p w
    , _lobbyCbAnnounceGame = announceGame w
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

type GameMap = Map.Map GameId GameRec

newGamesCb' :: MonadIO m => m GamesCb
newGamesCb' = do
  hc <- genHashCode'
  w <- liftIO $ newTVarIO Map.empty
  return GamesCb
    { _gamesCbHashCode = hc
    , _gamesCbInsertGame = insertGame w
    , _gamesCbFindGame = findGame w
    , _gamesCbRemoveGame = removeGame w
    }

insertGame :: TVar GameMap -> (GameId, GameRec) -> IO ()
insertGame w (gameId, rec) = atomically $ modifyTVar w (Map.insert gameId rec)

findGame :: TVar GameMap -> GameId -> IO (Maybe GameRec)
findGame w i = atomically $ do
  m <- readTVar w
  return $ Map.lookup i m

removeGame :: TVar GameMap -> GameId -> IO ()
removeGame w i = do
  m <- atomically $ readTVar w
  print $ Map.keys m
  let m' = Map.delete i m
  print $ Map.keys m'
  atomically $ writeTVar w m'

  -- atomically $ modifyTVar w (Map.delete i)

newResultsCb' :: MonadIO m => m ResultsCb
newResultsCb' = return (ResultsCb $ return ())

newGameCb' :: MonadIO m => m GameCb
newGameCb' = liftIO $ (,) <$> newChan <*> newChan
