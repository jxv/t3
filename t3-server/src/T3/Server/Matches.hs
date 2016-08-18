module T3.Server.Matches
  ( Matches
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar, modifyTVar, STM)
import Data.List (sortBy)
import Data.Map (Map)
import System.Random (randomIO, randoms, mkStdGen, StdGen)

newtype Matches = Matches (STM (Map String ()))
