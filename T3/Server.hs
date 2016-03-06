{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module T3.Server where

import qualified Data.Map as M
import Import
import T3.Session.Types
import T3.Comm.Class
import T3.Comm.Types

newtype Server a = Server { unServer :: ReaderT (TVar ServerData) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar ServerData))

data ServerData = ServerData
  { sdSessCfgs :: Map GameId SessionConfig
  , sdLobby :: [UserId]
  , sdMatchGame :: Map UserId GameId
  }
