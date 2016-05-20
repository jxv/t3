module T3.Server.Lobby.Types
  ( ListLobby
  ) where

import Control.Monad.STM.Class (TVar)
import Control.Monad.Conc.Class (STM)

import T3.Match.Types (UserName, StartCallback)

type ListLobby m = TVar (STM m) [(UserName, StartCallback m)]
