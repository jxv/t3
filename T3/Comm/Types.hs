module T3.Comm.Types
  ( UserId
  , Final(..)
  ) where

import Prelude
import Data.Text (Text)

type UserId = Text

data Final
  = Won
  | WonByDQ
  | Loss
  | LossByDQ
  | Tied
  deriving (Show, Eq)
