module T3.WebLang where

import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (Method, RequestHeaders, Status, ResponseHeaders)

data Request = Request
  { _reqMethod :: Method
  , _reqHeaders :: RequestHeaders
  , _reqBody :: ByteString
  } deriving (Show, Eq)

data Response = Response
  { _respStatus :: Status
  , _respHeaders :: ResponseHeaders
  , _respBody :: ByteString
  } deriving (Show, Eq)

class Monad m => Web m where
  play :: Request -> m Response
  start :: Request -> m Response
  randomHandler :: Request -> m Response
  register :: Request -> m Response
