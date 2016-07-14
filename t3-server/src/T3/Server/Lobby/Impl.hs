module T3.Server.Lobby.Impl
  ( addUserToLobby
  , userPairFromLobby
  ) where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Conc.Class
import Control.Monad.STM.Class
import Data.Maybe
import System.Random

import T3.Server (UserName, StartCallback)
import T3.Server.Lobby hiding (Lobby(..))

addUserToLobby :: (MonadSTM m, MonadConc m) => ListLobby m -> UserName -> StartCallback m -> m ()
addUserToLobby lobby un cb = atomically $ do
  lob <- readTVar lobby
  let shouldAdd = isNothing (lookup un lob)
  when shouldAdd $ writeTVar lobby ((un, cb) : lob)

userPairFromLobby :: (MonadSTM m, MonadConc m, MonadRandom m) => ListLobby m -> m (Maybe ((UserName, StartCallback m), (UserName, StartCallback m)))
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
