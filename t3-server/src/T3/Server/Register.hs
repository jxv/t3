module T3.Server.Register (main) where

import Control.Lens
import Data.Text (Text, pack)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..), asks)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant

import T3.Server.Gen (genToken', genUserId')
import T3.Server.Types

class Monad m => Registry m where
  register :: Name -> m Creds

class Monad m => UserStore m where
  addUser :: Name -> Token -> m UserId

class Monad m => Gen m where
  genToken :: m Token
  genUserId :: m UserId

class Monad m => RegistryState m where
  insertUser :: (Name, Token) -> m (Maybe UserId)

class Monad m => Failure m where
  insertUserFailure :: m a

main :: Registry m => RegisterReq -> m RegisterResp
main req = do
  let name = _registerReqName req
  creds <- register name
  return $ RegisterResp creds

register' :: (Gen m, UserStore m) => Name -> m Creds
register' name = do
  token <- genToken
  userId <- addUser name token
  return (Creds userId token)

addUser' :: (RegistryState m, Failure m, Gen m) => Name -> Token -> m UserId
addUser' name token = do
  mUserId <- insertUser (name, token)
  case mUserId of
    Nothing -> insertUserFailure
    Just userId -> return userId

--

instance Registry AppHandler where
  register = register'

instance UserStore AppHandler where
  addUser = addUser'

instance Gen AppHandler where
  genToken = genToken'
  genUserId = genUserId'

instance RegistryState AppHandler where
  insertUser = callback (_registryCbInsertUser . _envRegistryCb)

instance Failure AppHandler where
  insertUserFailure = throwError ServantErr
    { errHTTPCode = 400
    , errReasonPhrase = "Can't insert user"
    , errBody = ""
    , errHeaders = []
    }

