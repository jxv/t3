module T3.Server.HasCallbacksImpl
  ( getCallbacks
  , putCallbacks
  ) where

import Control.Monad.State (MonadState(..))
import Data.Map (Map)

import T3.Core (Loc)
import T3.Server (Step)
import T3.Server.Connection (Connection)

getCallbacks
  :: MonadState (Map Connection (m (Loc, Step -> m ()), Step -> m ())) m
  => m (Map Connection (m (Loc, Step -> m ()), Step -> m ()))
getCallbacks = get

putCallbacks
  :: MonadState (Map Connection (m (Loc, Step -> m ()), Step -> m ())) m 
  => Map Connection (m (Loc, Step -> m ()), Step -> m ())
  -> m ()
putCallbacks = put
