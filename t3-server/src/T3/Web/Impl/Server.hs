module T3.Web.Impl.Server where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Conc.Class (MonadConc(..))
import Control.Concurrent.STM (modifyTVar, readTVar, writeTVar)
import Control.Monad (mzero, forever)
import Data.Aeson
import Data.IORef
import Data.Maybe
import Control.Monad.Trans (MonadIO, liftIO)
import Safe (atMay)
import Network.HTTP.Types

import T3.Core
import T3.Web.Types (Request(..), Response(..))
import T3.Server.Types
import T3.Server.Class
import T3.Server.Dispatch.Types
import T3.Server.Lobby.Types
import T3.Match.Types

badRequest, badFormat, unauthorized :: Response
badRequest = Response status400 [] Nothing
badFormat = Response status400 [] (Just "FORMATTING ERROR")
unauthorized = Response status401 [] Nothing

-- /api/play/<match-id>/<match-token>
play :: ServerEsque m => Request -> m Response
play req = do
  let m = (,,)
        <$> (MatchId <$> atMay (_reqPath req) 2)
        <*> (MatchToken <$> atMay (_reqPath req) 3)
        <*> (decode $ _reqBody req)
  case m of
    Nothing -> return badFormat
    Just (matchId, matchToken, playReq) ->
      fromPlayResponse <$> playMove matchId matchToken playReq
  where
    fromPlayResponse :: Maybe PlayResponse -> Response
    fromPlayResponse (Just presp) = Response status200 [] (Just $ encode presp)
    fromPlayResponse Nothing = badRequest

-- /api/start
start :: ServerEsque m => Request -> m Response
start req = do
  let m = decode $ _reqBody req
  case m of
    Nothing -> return badFormat
    Just startReq -> fromStartResponse <$> startMatch startReq
  where
    fromStartResponse :: (Maybe StartResponse) -> Response
    fromStartResponse (Just sresp) = Response status200 [] (Just $ encode sresp)
    fromStartResponse Nothing = unauthorized

-- /api/random
randomHandler :: ServerEsque m => Request -> m Response
randomHandler req = do
  let m = decode $ _reqBody req
  case m of
    Nothing -> return badFormat
    Just startReq -> fromStartResponse <$> randomMatch startReq
  where
    fromStartResponse :: (Maybe StartResponse) -> Response
    fromStartResponse (Just sresp) = Response status200 [] (Just $ encode sresp)
    fromStartResponse Nothing = unauthorized

-- /api/register
register :: ServerEsque m => Request -> m Response
register req = do
  let m = decode (_reqBody req)
  case m of
    Nothing -> return badFormat
    Just rreq -> do
      let (UserName un) = _rreqName rreq
      if T.null un
        then return badRequest
        else fromRegisterResponse <$> registerUser rreq
  where
    fromRegisterResponse :: Maybe RegisterResponse -> Response
    fromRegisterResponse Nothing = badRequest
    fromRegisterResponse (Just rresp) = Response status200 [] (Just $ encode rresp)

