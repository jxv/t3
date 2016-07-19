module T3.Server.Lobby
  ( Lobby(..)
  ) where

import Control.Monad.STM.Class (TVar)
import Control.Monad.Conc.Class (STM)

import T3.Server (UserName, StartCallback, StartResponse)

class Monad m => Lobby m where
  addUserToLobby :: UserName -> m StartResponse
  userPairFromLobby :: m ((UserName, StartCallback m), (UserName, StartCallback m))
