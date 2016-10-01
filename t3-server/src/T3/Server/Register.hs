module T3.Server.Register
  ( Env(..)
  , main
  , run
  , Register
  ) where

import Control.Lens
import Data.Text (Text, pack)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..), asks)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant

import T3.Server.Gen (genToken', genUserId')
import T3.Server.Types (UserId, Name, Token, RegistryCb(..), Creds(..))

data Env = Env
  { _envName :: Name
  , _envRegistryCb :: RegistryCb
  }

newtype Register a = Register (ReaderT Env (ExceptT ServantErr IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError ServantErr, MonadIO)

run :: Register a -> Env -> ExceptT ServantErr IO a
run (Register m) env = runReaderT m env

main :: (Client m, Registry m) => m Creds
main = do
  name <- getName
  register name

class Monad m => Client m where
  getName :: m Name

getName' :: MonadReader Env m => m Name
getName' = asks _envName

instance Client Register where
  getName = getName'

class Monad m => Registry m where
  register :: Name -> m Creds

register' :: (Gen m, UserStore m) => Name -> m Creds
register' name = do
  token <- genToken
  userId <- addUser name token
  return (Creds userId token)

instance Registry Register where
  register = register'

class Monad m => UserStore m where
  addUser :: Name -> Token -> m UserId

addUser' :: (RegistryState m, Failure m, Gen m) => Name -> Token -> m UserId
addUser' name token = do
  mUserId <- insertUser (name, token)
  case mUserId of
    Nothing -> insertUserFailure name
    Just userId -> return userId

instance UserStore Register where
  addUser = addUser'

class Monad m => Gen m where
  genToken :: m Token
  genUserId :: m UserId

instance Gen Register where
  genToken = genToken'
  genUserId = genUserId'

class Monad m => RegistryState m where
  insertUser :: (Name, Token) -> m (Maybe UserId)

insertUser' :: (MonadReader Env m, MonadIO m) => (Name, Token) -> m (Maybe UserId)
insertUser' rec = asks (_registryCbInsertUser . _envRegistryCb) >>= \f -> liftIO (f rec)

instance RegistryState Register where
  insertUser = insertUser'

class Monad m => Failure m where
  insertUserFailure :: Name -> m a

insertUserFailure' :: MonadError ServantErr m => Name -> m a
insertUserFailure' _ = throwError ServantErr
  { errHTTPCode = 400
  , errReasonPhrase = "Can't insert user"
  , errBody = ""
  , errHeaders = []
  }

instance Failure Register where
  insertUserFailure = insertUserFailure'
