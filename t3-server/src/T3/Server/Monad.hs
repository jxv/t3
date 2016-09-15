module T3.Server.Monad
  ( Server
  , runServer
  ) where

import Control.Monad.IO.Class (MonadIO)

newtype Server a = Server { unServer :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runServer :: Server a -> IO a
runServer = unServer
