{-# LANGUAGE TemplateHaskell #-}
module T3.Server.Register
  ( main
  ) where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import T3.Server.Types (UserId, Token)

class Monad m => Client m where
  getUserId :: m UserId

class Monad m => Registry m where
  createUser :: UserId -> m ()

main :: (Registry m, Client m) => m ()
main = do
  userId <- getUserId
  createUser userId

data Env = Env
  { _envGetUserId :: IO UserId
  , _envCreateUser :: UserId -> IO ()
  }

makeClassy ''Env

newtype Register a = Register (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance Client Register where
  getUserId = view envGetUserId >>= liftIO

instance Registry Register where
  createUser u = view envCreateUser >>= \f -> liftIO $ f u
