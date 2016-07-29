module T3.Server.HasTimeoutLimitImpl
  ( getTimeoutLimit
  ) where

import Control.Monad.State (MonadState(..))
import T3.Server.Milliseconds (Milliseconds)

getTimeoutLimit :: MonadState (Maybe Milliseconds) m => m (Maybe Milliseconds)
getTimeoutLimit = get
