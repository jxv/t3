module T3.Server.Shared.RegistryObject
  ( newRegistryObject'
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

newRegistryObject' :: MonadIO m => m RegistryObject
newRegistryObject' = do
  hc <- genHashCode'
  w <- liftIO $ newTVarIO Map.empty
  return RegistryObject
    { _registryObjectHashCode = hc
    , _registryObjectInsertUser = insertUser w
    , _registryObjectGetUserById = getUserById w
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
