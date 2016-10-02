{-# LANGUAGE TemplateHaskell #-}
module T3.Server.Control.Types
  ( Env(..)
  , HasEnv(..)
  , AppHandler(..)
  , callback
  ) where

import Control.Monad (mzero)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import Data.String (IsString)
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), Value(..), (.=), object)
import Servant

import T3.Server.Types

data Env = Env
  { _envPort :: Int
  , _envLobbyCb :: LobbyCb
  , _envGamesCb :: GamesCb
  , _envResultsCb :: ResultsCb
  , _envRegistryCb :: RegistryCb
  }

newtype AppHandler a = AppHandler { runHandler :: ReaderT Env (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError ServantErr, MonadIO)

callback :: (Env -> b -> IO a) -> b -> AppHandler a
callback x i = do
  f <- asks x
  liftIO (f i)

makeClassy ''Env
