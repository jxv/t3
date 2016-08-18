module T3.GameConsole.Console
  ( Console(..)
  ) where

import Prelude hiding (getLine, putStrLn)

class Monad m => Console m where
  putStrLn :: String -> m ()
  getLine :: m String
