{-# LANGUAGE DeriveFunctor #-}
module T3.Game.Types
  ( Win(..)
  , Lose(..)
  ) where

import Prelude

newtype Win a = Win a
  deriving (Functor)

newtype Lose a = Lose a
  deriving (Functor)
