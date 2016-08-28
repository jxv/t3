{-# LANGUAGE DeriveFunctor #-}
module T3.Game.Types
  ( Win(..)
  , Lose(..)
  , Step(..)
  , Final(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Text (Text)

import T3.Core (Board)

newtype Win a = Win a
  deriving (Eq, Show, Functor)

newtype Lose a = Lose a
  deriving (Eq, Show, Functor)

data Step = Step
  { _stepBoard :: Board
  , _stepFinal :: Maybe Final
  } deriving (Show, Eq)

data Final
  = Won
  | WonByDQ
  | Loss
  | LossByDQ
  | Tied
  deriving (Show, Eq)

instance FromJSON Final where
  parseJSON (String "Won") = pure Won
  parseJSON (String "WonByDQ") = pure WonByDQ
  parseJSON (String "Loss") = pure Loss
  parseJSON (String "LossByDQ") = pure LossByDQ
  parseJSON (String "Tied") = pure Tied
  parseJSON _ = mzero

instance ToJSON Final where
  toJSON f = String $ case f of
    Won -> "Won"
    WonByDQ -> "WonByDQ"
    Loss -> "Loss"
    LossByDQ -> "LossByDQ"
    Tied -> "Tied"
