module T3.Match.Console
  ( Console(..)
  ) where

import Data.Text (Text)

class Monad m => Console m where
  printStdout :: Text -> m ()
