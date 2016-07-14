module T3.Registrar.Impl
  ( register
  , authenticate
  ) where

import qualified Data.Map as M (insert, member, lookup, Map)
import Control.Monad (unless)
import Control.Monad.Conc.Class (atomically, MonadConc(..))
import Control.Concurrent.Classy.STM (readTVar, writeTVar)
import T3.Server (UserCreds(..), UserKey(..), UserName(..))
import T3.Server.Part.Impl (Server(..))

register :: Server IO -> UserName -> UserKey -> IO ()
register srv name userKey = atomically $ do
  users <- readTVar (_srvUsers srv)
  let users' = M.insert name userKey users
  unless (M.member name users) $ writeTVar (_srvUsers srv) users'

authenticate :: MonadConc m => Server m -> UserCreds -> STM m Bool
authenticate srv uc = do
  users <- readTVar (_srvUsers srv)
  return $ M.lookup (_ucName uc) users == Just (_ucKey uc)
