{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified T3.Server.Practice as PracticeLobby
import T3.Server.Types

type AppServer api = ServerT api AppHandler

toHandler :: Env -> AppHandler api -> Handler api
toHandler env appHandler = runReaderT (runHandler appHandler) env

type API =
  Register :<|>
  PracticeLobby

type Register = "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] RegisterResp
type PracticeLobby = "practice-lobby" :> ReqBody '[JSON] LobbyReq :> Post '[JSON] LobbyResp

register :: RegisterReq -> AppHandler RegisterResp
register = Register.main

practiceLobby :: LobbyReq -> AppHandler LobbyResp
practiceLobby = PracticeLobby.main

serverT :: AppServer API
serverT = register :<|> practiceLobby

appToHandler' :: forall a. Env -> AppHandler a -> Handler a
appToHandler' env (AppHandler m) = runReaderT m env

appToHandler :: Env -> AppHandler :~> Handler
appToHandler env = Nat (appToHandler' env)

server :: Env -> Server API
server env = enter (appToHandler env) serverT

application :: Env -> Application
application env = serve (Proxy :: Proxy API) (server env)

main :: MonadIO m => Env -> m ()
main env = liftIO $ run 8080 (application env)
