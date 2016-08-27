module T3.GameConsole.Parts
  ( Console(..)
  ) where

import Prelude hiding (getLine, putStrLn)

class Monad m => Console m where
  putStrLn :: String -> m ()
  getLine :: m String
