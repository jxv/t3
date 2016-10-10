module T3.Server.Shared.GameObject
  ( newGameObject'
  ) where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import T3.Server.Gen (genUserId', genHashCode')
import T3.Server.Types

newGameObject' :: MonadIO m => m GameObject
newGameObject' = liftIO $ GameObject <$> newChan <*> newChan
