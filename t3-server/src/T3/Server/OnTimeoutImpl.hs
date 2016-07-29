module T3.Server.OnTimeoutImpl
  ( onTimeout
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither)
import Control.Monad.Conc.Class (MonadConc, threadDelay)
import Control.Concurrent.Async (race)
import T3.Server.Milliseconds (Milliseconds(..))

delay :: MonadConc m => Milliseconds -> m ()
delay (Milliseconds ms) = threadDelay (scale * ms)
  where
    scale = 1000

onTimeout :: (MonadConc m) => m a -> Milliseconds -> m (Maybe a)
onTimeout callee ms = either Just (const Nothing) <$> race callee (delay ms)
