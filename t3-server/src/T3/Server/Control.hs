{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module T3.Server.Control
  ( Env(..)
  , main
  ) where

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (fromList)
import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

import qualified T3.Server.Register as Register
import qualified T3.Server.PracticeLobby as PracticeLobby
import T3.Server.Types
import T3.Server.Control.Types

main :: MonadIO m => Env -> m ()
main env = liftIO $ run (_envPort env) (application env)

application :: Env -> Application
application env = serve (Proxy :: Proxy API) (server env)

server :: Env -> Server API
server env = enter (appToHandler env) serverT
  where
    appToHandler :: Env -> AppHandler :~> Handler
    appToHandler env = Nat (appToHandler' env)
      where
        appToHandler' :: forall a. Env -> AppHandler a -> Handler a
        appToHandler' env (AppHandler m) = runReaderT m env

type AppServer api = ServerT api AppHandler

type Register = "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] RegisterResp
type PracticeLobby = "practice-lobby" :> ReqBody '[JSON] LobbyReq :> Post '[JSON] LobbyResp

type API =
  Register :<|>
  PracticeLobby

serverT :: AppServer API
serverT =
  Register.main :<|>
  PracticeLobby.main
