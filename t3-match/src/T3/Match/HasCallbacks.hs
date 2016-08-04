module T3.Match.HasCallbacks
  ( HasCallbacks(..)
  ) where

import Data.Map (Map)
import T3.Core (Loc, XO)
import T3.Match.Types (Step)

class Monad m => HasCallbacks m where
  getCallbacks :: m (Map XO (m (Loc, Step -> m ()), Step -> m ()))
  putCallbacks :: Map XO (m (Loc, Step -> m ()), Step -> m ()) -> m ()
