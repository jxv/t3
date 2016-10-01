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
import T3.Server.Types

data Env = Env
  { _envPort :: Int
  , _envRegistryCb :: RegistryCb
  }

type AppServer api = ServerT api AppHandler

newtype AppHandler a = AppHandler { runHandler :: ReaderT Env (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError ServantErr, MonadIO)

toHandler :: Env -> AppHandler api -> Handler api
toHandler env appHandler = runReaderT (runHandler appHandler) env

type Register = "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] RegisterResp

type API = Register

register :: RegisterReq -> AppHandler RegisterResp
register req = do
  registryCb <- asks _envRegistryCb
  let env = Register.Env (Name (_registerReqName req)) registryCb
  creds <- AppHandler . lift $ Register.run Register.main env
  return $ RegisterResp creds

serverT :: AppServer API
serverT = register

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
