module T3.Server.Shared.ResultsObject
  ( newResultsObject'
  ) where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import T3.Server.Gen (genUserId', genHashCode')
import T3.Server.Types

newResultsObject' :: MonadIO m => m ResultsObject
newResultsObject' = liftIO $ do
  w <- newTVarIO Map.empty
  return ResultsObject
    { _resultsObjectSaveResult = saveResult w
    , _resultsObjectFindResult = findResult w
    }

type ResultMap = Map.Map GameId Result

saveResult :: TVar ResultMap -> Result -> IO ()
saveResult w r = atomically . modifyTVar w $
  Map.insert (_gameStartGameId . _resultGameStart $ r) r

findResult :: TVar ResultMap -> GameId -> IO (Maybe Result)
findResult w gid = atomically $ do
  m <- readTVar w
  return $ Map.lookup gid m
