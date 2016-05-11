module T3.Util
  ( genBase64
  ) where

import qualified Data.Text as T
import Control.Monad.Random
import Data.Text (Text)

genBase64 :: MonadRandom m => Int -> m Text
genBase64 n = fmap T.pack (sequence $ replicate n gen)
  where
    gen = fmap (\x -> vals !! (mod x len)) getRandom
    len = length vals
    vals = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-','_']
