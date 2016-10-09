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
newResultsObject' = return (ResultsObject $ return ())
