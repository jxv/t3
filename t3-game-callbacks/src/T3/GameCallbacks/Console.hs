module T3.GameCallbacks.Console
  ( printStdout
  ) where

import qualified Data.Text.IO as T (putStrLn)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(..))

printStdout :: MonadIO m => Text -> m ()
printStdout msg = liftIO $ T.putStrLn msg
