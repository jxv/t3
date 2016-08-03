module T3.Http.Serve
  ( serve
  ) where

import Network.HTTP.Types.Status (status404)
import T3.Http (Request(..), Response(..), Http(..))

serve :: Http m => Request -> m Response
serve request =  case _reqPath request of
  ["play"] -> play request
  ["start"] -> start request
  ["random"] -> randomHandler request
  ["match"] -> match request
  _ -> return response404
  where
    response404 ::Response
    response404 = Response
      { _respStatus = status404
      , _respHeaders = []
      , _respBody = Nothing
      }
