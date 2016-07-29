module T3.Server.Console
  ( Console(..)
  ) where

import Data.Text (Text)

class Monad m => Console m where
  printStdout :: Text -> m ()
