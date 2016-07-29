module T3.Server.Milliseconds
  ( Milliseconds(..)
  ) where

newtype Milliseconds = Milliseconds Int
  deriving (Show, Eq, Ord, Num)
