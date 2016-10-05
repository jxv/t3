module T3.Server.SharedCb
  ( newRegistryCb'
  , newLobbyCb'
  , newGamesCb'
  ) where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Control.Concurrent.MVar

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
  return LobbyCb
    { _lobbyCbTransferUser = transferUser p
    }

type UserQueue = [(UserId, MVar GameId)]

transferUser :: TVar UserQueue -> UserId -> IO (Maybe GameId)
transferUser p i = do
  ref <- newEmptyMVar
  atomically $ modifyTVar p $ \q -> q ++ [(i, ref)]
  gameId <- takeMVar ref
  return $ Just gameId

type GameMap = Map.Map GameId ThreadCb

newGamesCb' :: MonadIO m => m GamesCb
newGamesCb' = do
  hc <- genHashCode'
  w <- liftIO $ newTVarIO Map.empty
  return GamesCb
    { _gamesCbHashCode = hc
    , _gamesCbInsertGame = insertGame w
    }

insertGame :: TVar GameMap -> (GameId, ThreadCb) -> IO ()
insertGame w (gameId, threadCb) = atomically $ modifyTVar w (Map.insert gameId threadCb)
