module T3.Match.HasCallbacks
  ( HasCallbacks(..)
  ) where

import Data.Map (Map)
import T3.Core (Loc)
import T3.Match.Types (Step)
import T3.Match.Connection (Connection)

class Monad m => HasCallbacks m where
  getCallbacks :: m (Map Connection (m (Loc, Step -> m ()), Step -> m ()))
  putCallbacks :: Map Connection (m (Loc, Step -> m ()), Step -> m ()) -> m ()
