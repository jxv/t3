{-# LANGUAGE TemplateHaskell #-}
module T3.Server.Register
  ( main
  ) where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types (UserId(..), Name, Token(..), RegistryCb(..))

data Env = Env
  { _envGetName :: Name
  , _envRegistryCb :: RegistryCb IO
  , _envRespond :: UserId -> Token -> IO ()
  }

class Monad m => Client m where
  getName :: m Name
  respond :: UserId -> Token -> m ()

class Monad m => Registry m where
  createUser :: Name -> m (UserId, Token)

class Monad m => UserStore m where
  addUser :: Name -> Token -> m UserId

class Monad m => Gen m where
  genToken :: m Token

class Monad m => RegistryState m where
  insertUser :: Name -> Token -> m (Maybe UserId)

main :: (Registry m, Client m) => m (UserId, Token)
main = do
  name <- getName
  createUser name

createUser' :: (Gen m, UserStore m) => Name -> m (UserId, Token)
createUser' name = do
  token <- genToken
  userId <- addUser name token
  return (userId, token)

addUser' :: (RegistryState m, MonadError () m) => Name -> Token -> m UserId
addUser' name token = do
  mUserId <- insertUser name token
  case mUserId of
    Just userId -> return userId
    Nothing -> throwError ()

newtype Register a = Register (ExceptT () (ReaderT Env IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ())

instance Client Register where
  getName = asks _envGetName
  respond userId token = asks _envRespond >>= \f -> liftIO $ f userId token

instance Registry Register where
  createUser = createUser'

instance Gen Register where
  genToken = return $ Token "4"

instance UserStore Register where
  addUser = addUser'

instance RegistryState Register where
  insertUser name token = do
    f <- asks (_registryCbInsertUser . _envRegistryCb)
    liftIO $ f name token
