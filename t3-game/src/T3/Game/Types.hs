{-# LANGUAGE DeriveFunctor #-}
module T3.Game.Types
  ( Win(..)
  , Lose(..)
  ) where

newtype Win a = Win a
  deriving (Eq, Show, Functor)

newtype Lose a = Lose a
  deriving (Eq, Show, Functor)
