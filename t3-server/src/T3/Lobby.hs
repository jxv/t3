module T3.Lobby
  ( ListLobby
  , Lobby(..)
  ) where

import Control.Monad.STM.Class (TVar)
import Control.Monad.Conc.Class (STM)

import T3.Match -- types

type ListLobby m = TVar (STM m) [(UserName, StartCallback m)]

class Monad m => Lobby m where
  addUserToLobby :: UserName -> StartCallback m -> m Bool
  userPairFromLobby :: m (Maybe ((UserName, StartCallback m), (UserName, StartCallback m)))
