module T3.RegistrarImpl
  ( register
  , authenticate
  ) where

import qualified Data.Map as M (insert, member, lookup, Map)
import Control.Monad (unless)
import Control.Monad.Conc.Class (atomically, MonadConc(..))
import Control.Concurrent.Classy.STM (readTVar, writeTVar)
import T3.Server (UserCreds(..), UserKey(..), UserName(..))

class Monad m => HasUsers m where
  getUsers :: m (M.Map UserName UserKey)
  -- users <- readTVar (_srvUsers srv)
  putUsers :: M.Map UserName UserKey -> m ()
  -- writeTVar (_srvUsers srv) users

register :: HasUsers m => UserName -> UserKey -> m ()
register name userKey = do
  users <- getUsers
  let users' = M.insert name userKey users
  unless (M.member name users) $ putUsers users'

authenticate :: HasUsers m => UserCreds -> m Bool
authenticate uc = do
  users <- getUsers
  return $ M.lookup (_ucName uc) users == Just (_ucKey uc)
