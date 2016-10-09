module T3.Server.Control.Register
  ( UserStore(..)
  , Gen(..)
  , RegistryState(..)
  , Failure(..)
  , register
  , addUser'
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
import T3.Server.Types
import T3.Server.Control.Types

class Monad m => UserStore m where
  addUser :: Name -> Token -> m UserId

class Monad m => Gen m where
  genToken :: m Token
  genUserId :: m UserId

class Monad m => RegistryState m where
  insertUser :: (Name, Token) -> m (Maybe UserId)

class Monad m => Failure m where
  insertUserFailure :: m a

register :: (Gen m, UserStore m) => RegisterReq -> m RegisterResp
register (RegisterReq name) = do
  token <- genToken
  userId <- addUser name token
  return $ RegisterResp (Creds userId token)

addUser' :: (RegistryState m, Failure m, Gen m) => Name -> Token -> m UserId
addUser' name token = try insertUserFailure $ insertUser (name, token)

instance UserStore AppHandler where
  addUser = addUser'

instance Gen AppHandler where
  genToken = genToken'
  genUserId = genUserId'

instance RegistryState AppHandler where
  insertUser = callback (_registryObjectInsertUser . _envRegistryObject)

instance Failure AppHandler where
  insertUserFailure = throwError ServantErr
    { errHTTPCode = 400
    , errReasonPhrase = "Can't insert user"
    , errBody = ""
    , errHeaders = []
    }
