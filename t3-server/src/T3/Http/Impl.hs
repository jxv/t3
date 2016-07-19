module T3.Http.Impl
  ( playHandler
  , startHandler
  , randomHandler
  , registerHandler
  ) where

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
import Data.Text (Text)
import Data.Text.Conversions
import Control.Monad.Trans (MonadIO, liftIO)
import Safe (atMay)
import Network.HTTP.Types

import T3.Http hiding (Http(..))
import T3.Server
  ( Server(..)
  , StartResponse(..)
  , RegisterRequest(..)
  , RegisterResponse(..)
  , PlayResponse(..)
  , UserName(..)
  , MatchId(..), MatchToken(..)
  ) 

badRequest, badFormat, unauthorized :: Response
badRequest = Response status400 [] Nothing
badFormat = Response status400 [] (Just "FORMATTING ERROR")
unauthorized = Response status401 [] Nothing

lookupHeaderText :: HeaderName -> RequestHeaders -> Maybe Text
lookupHeaderText header headers = do
  raw <- lookup header headers
  decodeConvertText (UTF8 raw)

-- /play
playHandler :: Server m => Request -> m Response
playHandler req = do
  let headers = _reqHeaders req
  let mMatchId = MatchId <$> lookupHeaderText "x-match-id" headers
  let mMatchToken = MatchToken <$> lookupHeaderText "x-match-token" headers
  let mPlayMove = (,,) <$> mMatchId <*> mMatchToken <*> (decode $ _reqBody req)
  case mPlayMove of
    Nothing -> return badFormat
    Just (matchId, matchToken, playReq) -> response <$> playMove matchId matchToken playReq
  where
    response :: PlayResponse -> Response
    response presp = Response status200 [] (Just $ encode presp)

-- /start
startHandler :: Server m => Request -> m Response
startHandler req = do
  let m = decode $ _reqBody req
  case m of
    Nothing -> return badFormat
    Just startReq -> response <$> startMatch startReq
  where
    response :: StartResponse -> Response
    response sresp = Response status200 [] (Just $ encode sresp)

-- /random
randomHandler :: Server m => Request -> m Response
randomHandler req = do
  let m = decode $ _reqBody req
  case m of
    Nothing -> return badFormat
    Just startReq -> response <$> randomMatch startReq
  where
    response :: StartResponse -> Response
    response sresp = Response status200 [] (Just $ encode sresp)

-- /register
registerHandler :: Server m => Request -> m Response
registerHandler req = do
  let m = decode (_reqBody req)
  case m of
    Nothing -> return badFormat
    Just rreq -> do
      let (UserName un) = _rreqName rreq
      if T.null un
        then return badRequest
        else response <$> registerUser rreq
  where
    response :: RegisterResponse -> Response
    response rresp = Response status200 [] (Just $ encode rresp)
