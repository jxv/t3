module T3.Server.HasConnection
  ( HasConnection(..)
  ) where

import T3.Core (XO)
import T3.Server.Connection (Connection)

class Monad m => HasConnection m where
  getConnection :: XO -> m Connection
