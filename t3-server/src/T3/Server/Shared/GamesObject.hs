module T3.Server.Shared.GamesObject
  ( newGamesCb'
  ) where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import T3.Server.Gen (genUserId', genHashCode')
import T3.Server.Types

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
removeGame w i = atomically $ modifyTVar w (Map.delete i)
