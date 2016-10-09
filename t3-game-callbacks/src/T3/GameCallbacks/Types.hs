module T3.GameCallbacks.Types
  ( Step(..)
  , Final(..)
  ) where

import T3.Core (Board)

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
