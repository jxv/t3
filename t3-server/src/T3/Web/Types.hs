module T3.Web.Types
  ( Request(..)
  , Response(..)
  ) where

import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (Method, RequestHeaders, Status, ResponseHeaders)

data Request = Request
  { _reqMethod :: Method
  , _reqPath :: [Text]
  , _reqHeaders :: RequestHeaders
  , _reqBody :: ByteString
  } deriving (Show, Eq)

data Response = Response
  { _respStatus :: Status
  , _respHeaders :: ResponseHeaders
  , _respBody :: Maybe ByteString
  } deriving (Show, Eq)


