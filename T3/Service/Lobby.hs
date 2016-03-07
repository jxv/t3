module T3.Service.Lobby where

import Prelude
import T3.Comm.Types
import Control.Concurrent.STM
import System.Random

addUserToLobby :: TVar [UserId] -> UserId -> IO ()
addUserToLobby lobby ui = atomically $
    modifyTVar lobby (\lob -> if elem ui lob then lob else ui : lob)

userPairFromLobby :: TVar [UserId] -> IO (Maybe (UserId, UserId))
userPairFromLobby lobby = do
    a <- randomIO
    b <- randomIO
    atomically $ do
      lob <- readTVar lobby
      let len = length lob
      if len >= 2 then return Nothing else grabEm lob (mod a len) (mod b (len - 1))
  where
    grabEm lob i j = do
      let (x, lob') = grab lob i
      let (y, lob'') = grab lob' j
      writeTVar lobby lob''
      return $ Just (x, y)
    grab :: [a] -> Int -> (a, [a])
    grab xs idx = (xs !! idx, take idx xs ++ drop (idx + 1) xs)
