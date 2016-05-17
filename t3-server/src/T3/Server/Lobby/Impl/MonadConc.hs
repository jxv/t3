module T3.Server.Lobby.Impl.MonadConc where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Conc.ClassTmp
import Control.Concurrent.STM (TVar, STM, readTVar, writeTVar)
import System.Random
import Data.Maybe

import T3.Server.Lobby.Types
import T3.Match.Types

addUserToLobby :: MonadConc m => ListLobby m -> UserName -> StartCallback m -> m Bool
addUserToLobby lobby un cb = atomically $ do
  lob <- readTVar lobby
  let shouldAdd = isNothing (lookup un lob)
  when shouldAdd $ writeTVar lobby ((un, cb) : lob)
  return shouldAdd

userPairFromLobby :: (MonadConc m, MonadRandom m) => ListLobby m -> m (Maybe ((UserName, StartCallback m), (UserName, StartCallback m)))
userPairFromLobby lobby = do
  a <- getRandom
  b <- getRandom
  atomically $ do
    lob <- readTVar lobby
    let len = length lob
    if len < 2 then return Nothing else grabEm lob (mod a len) (mod b (len - 1))
  where
    grabEm lob i j = do
      let (x, lob') = grab lob i
      let (y, lob'') = grab lob' j
      writeTVar lobby lob''
      return $ Just (x, y)
    grab :: [a] -> Int -> (a, [a])
    grab xs idx = (xs !! idx, take idx xs ++ drop (idx + 1) xs)
