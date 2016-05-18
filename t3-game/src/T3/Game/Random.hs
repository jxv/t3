module T3.Game.Random
  ( randomLoc
  ) where

import Data.Map (keys)
import T3.Game.Core
import Control.Monad.Random
import Data.List (notElem)

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
