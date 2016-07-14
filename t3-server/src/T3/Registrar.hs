module T3.Registrar
  ( Registrar(..)
  ) where

import T3.Server (UserCreds, UserKey, UserName)

class Monad m => Registrar m where
  register :: UserName -> UserKey -> m ()
  authenticate :: UserCreds -> m ()
