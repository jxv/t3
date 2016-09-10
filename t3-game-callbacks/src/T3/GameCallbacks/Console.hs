module T3.GameCallbacks.Console
  ( Console(..)
  , printStdout'
  ) where

import qualified Data.Text.IO as T (putStrLn)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)

class Monad m => Console m where
  printStdout :: Text -> m ()

printStdout' :: MonadIO m => Text -> m ()
printStdout' msg = liftIO $ T.putStrLn msg
