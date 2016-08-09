module T3.Server.Lobby
  ( Lobby
  , empty
  , addUser
  , getUsers
  ) where

import Data.List (sortBy)
import System.Random (randomIO, randoms, mkStdGen, StdGen)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar, modifyTVar, STM)

type Value = (String, ())

newtype Lobby = Lobby (TVar [Value])

empty :: IO Lobby
empty = Lobby <$> newTVarIO []

addUser :: Lobby -> Value -> IO ()
addUser (Lobby tvarList) user = atomically $ modifyTVar tvarList (user:)

getUsers :: Lobby -> IO (Maybe (Value, Value))
getUsers lobby = do
  seed <- randomIO
  atomically $ pickPair lobby seed

pickPair :: Lobby -> Int -> STM (Maybe (Value, Value))
pickPair (Lobby tvarList) seed = do
  let gen = mkStdGen seed
  list <- readTVar tvarList
  case splitHeadPair (shuffle gen list) of
    Nothing -> return Nothing
    Just (pair, list') -> do
      writeTVar tvarList list'
      return $ Just pair

splitHeadPair :: [a] -> Maybe ((a, a), [a])
splitHeadPair (a:b:rest) = Just ((a,b), rest)
splitHeadPair _ = Nothing

shuffle :: StdGen -> [a] -> [a]
shuffle gen list = map snd $ sortBy (\a b -> compare (fst a) (fst b)) $ zip (randoms gen :: [Integer]) list
