module T3.GameConsole.ConsoleImpl
  ( putStrLn
  , getLine
  ) where

import qualified Prelude as IO (putStrLn, getLine)
import Prelude hiding (putStrLn, getLine)
import Control.Monad.IO.Class (MonadIO(liftIO))

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . IO.putStrLn

getLine :: MonadIO m => m String
getLine = liftIO IO.getLine
