module T3.Registrar
  ( Registrar(..)
  ) where

import T3.Server (UserCreds, UserKey)
import T3.Match (UserName)

class Monad m => Registrar m where
  register :: UserName -> UserKey -> m ()
  authenticate :: UserCreds -> m ()
