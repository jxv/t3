module T3.Server.Lobby.Types
  ( ListLobby
  ) where

import Control.Concurrent.STM (TVar)

import T3.Server.Dispatch.Class
import T3.Match

type ListLobby m = TVar [(UserName, StartCallback m)]
