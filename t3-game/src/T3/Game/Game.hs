{-# LANGUAGE DeriveFunctor #-}
module T3.Game.Game
  ( Win(..)
  , Lose(..)
  , Game(..)
  ) where

import T3.Core (Loc, XO, Board)

newtype Win a = Win a
  deriving (Eq, Show, Functor)

newtype Lose a = Lose a
  deriving (Eq, Show, Functor)

class Monad m => Game m where
  move :: XO -> m Loc
  forfeit :: Win XO -> Lose XO -> m ()
  end :: Win XO -> Lose XO -> m ()
  tie :: m ()
