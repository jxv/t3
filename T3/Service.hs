module T3.Service where

import Prelude
import T3.Game
import T3.Game.Types
import T3.Comm.Types
import Data.Text (Text)

type SessionId = Text

class Monad m => Service m where
 newSession :: UserId -> m SessionId

start :: (Game m, Service m) => UserId -> m SessionId
start ui = newSession ui

play :: Service m => m ()
play = return ()
