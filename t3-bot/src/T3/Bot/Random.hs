module T3.Bot.Random
  ( randomLoc
  ) where

import Data.Map (keys)
import Control.Monad.Random (MonadRandom(getRandom))
import Data.List (notElem)

import T3.Core

randomLoc :: MonadRandom m => Board -> m (Maybe Loc)
randomLoc b 
  | null available = return Nothing
  | otherwise = do
      r <- getRandom
      let idx = r `mod` (length available)
      return $ Just (available !! idx)
  where
    available = filter (flip notElem taken) allLocs
    taken = keys (boardMap b)
    allLocs = [Loc x y | x <- [0..2], y <- [0..2]]
