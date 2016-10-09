{-# LANGUAGE DeriveFunctor #-}

module T3.Game.Types
  ( Win(..)
  , Lose(..)
  , Step(..)
  , Final(..)
  ) where

import Control.Monad (mzero)
import Data.Text (Text)
import T3.Core (Board)

newtype Win a = Win a
  deriving (Eq, Show, Functor)

newtype Lose a = Lose a
  deriving (Eq, Show, Functor)

data Step = Step
  { _stepBoard :: !Board
  , _stepFinal :: !(Maybe Final)
  } deriving (Show, Eq)

data Final
  = Won
  | WonByDQ
  | Loss
  | LossByDQ
  | Tied
  deriving (Show, Eq)
