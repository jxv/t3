module T3.Server.HasConnectionImpl
  ( getConnection
  , Connections(..)
  ) where

import Control.Monad.Reader (MonadReader, asks)
import T3.Core (XO(..))
import T3.Server.Connection (Connection)

data Connections = Connections
  { _connectionsX :: Connection
  , _connectionsO :: Connection
  } deriving (Show, Eq)

getConnection :: MonadReader Connections m => XO -> m Connection
getConnection X = asks _connectionsX
getConnection O = asks _connectionsO
