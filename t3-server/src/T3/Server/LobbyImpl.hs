module T3.Server.LobbyImpl
  ( addUserToLobby
  , userPairFromLobby
  ) where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Conc.Class
import Control.Monad.STM.Class
import Data.Maybe
import System.Random

import T3.Server (UserName, StartCallback, StartResponse(..))
import T3.Server.Util (toGameState)
import T3.Server.Lobby hiding (Lobby(..))

type ListLobby m = TVar (STM m) [(UserName, StartCallback m)]

addUserToLobby :: (MonadSTM m, MonadConc m) => ListLobby m -> UserName -> m StartResponse
addUserToLobby lobby un = do
  resp <- newEmptyMVar
  let startCallback matchInfo users step = putMVar resp $ StartResponse matchInfo users (toGameState step)
  atomically $ do
    lob <- readTVar lobby
    let shouldAdd = isNothing (lookup un lob)
    when shouldAdd $ writeTVar lobby ((un, startCallback) : lob)
  takeMVar resp

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
