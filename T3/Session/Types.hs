module T3.Session.Types where

import Prelude
import Data.Text (Text)
import T3.Game.Core
import T3.Game.Types
import T3.Comm.Types

type GameId = Text
type GameToken = Text

data SessionConfig = SessionConfig
  { sessCfgX :: (UserId, GameToken)
  , sessCfgO :: (UserId, GameToken)
  } deriving (Show, Eq)
