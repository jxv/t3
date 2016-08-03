module T3.Match.HasConnection
  ( HasConnection(..)
  ) where

import T3.Core (XO)
import T3.Match.Connection (Connection)

class Monad m => HasConnection m where
  getConnection :: XO -> m Connection
