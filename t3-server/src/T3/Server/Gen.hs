module T3.Server.Gen
  ( genText
  , genUserId'
  , genToken'
  , genHashCode'
  ) where

import Data.Text (Text, pack)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Random

import T3.Server.Types

genText :: MonadIO m => Int -> m Text
genText 0 = return ""
genText n = liftIO $ do
  str <- sequenceA $ replicate n (randomRIO ('a','z'))
  return $ pack str

genUserId' :: MonadIO m => m UserId
genUserId' = UserId <$> genText 12

genToken' :: MonadIO m => m Token
genToken' = Token <$> genText 12

genHashCode' :: MonadIO m => m HashCode
genHashCode' = HashCode <$> genText 12
