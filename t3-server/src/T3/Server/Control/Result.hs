module T3.Server.Control.Result
  ( result
  ) where

import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks)
import Control.Monad.Except (ExceptT, MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant

import T3.Server.Types
import T3.Server.Control.Types

result :: (MonadReader Env m, MonadIO m) => ResultReq -> m ResultResp
result (ResultReq gid) = do
  f <- asks (_resultsObjectFindResult . _envResultsObject)
  mr <- liftIO $ f gid
  case mr of
    Nothing -> error "Can't find result"
    Just r -> return $ ResultResp r
