module T3.Server.Shared.GamesObject
  ( newGamesObject'
  ) where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import T3.Server.Gen (genUserId', genHashCode')
import T3.Server.Types

type GameMap = Map.Map GameId GameRecord

newGamesObject' :: MonadIO m => m GamesObject
newGamesObject' = do
  hc <- genHashCode'
  w <- liftIO $ newTVarIO Map.empty
  return GamesObject
    { _gamesObjectHashCode = hc
    , _gamesObjectInsertGame = insertGame w
    , _gamesObjectFindGame = findGame w
    , _gamesObjectRemoveGame = removeGame w
    }

insertGame :: TVar GameMap -> (GameId, GameRecord) -> IO ()
insertGame w (gameId, rec) = atomically $ modifyTVar w (Map.insert gameId rec)

findGame :: TVar GameMap -> GameId -> IO (Maybe GameRecord)
findGame w i = atomically $ do
  m <- readTVar w
  return $ Map.lookup i m

removeGame :: TVar GameMap -> GameId -> IO ()
removeGame w i = atomically $ modifyTVar w (Map.delete i)
