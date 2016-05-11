module T3.Server.Lobby where

import Control.Concurrent.STM (TVar)
import T3.Match

type ListLobby m = TVar [(UserName, StartCallback m)]

class Monad m => Lobby m where
  addUserToLobby :: ListLobby m -> UserName -> StartCallback m -> m Bool
  userPairFromLobby :: ListLobby m -> m (Maybe ((UserName, StartCallback m), (UserName, StartCallback m)))
