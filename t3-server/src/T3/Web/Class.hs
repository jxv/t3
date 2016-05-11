module T3.Web.Class
  ( Web(..)
  , FromRequest(..)
  , ToResponse(..)
  ) where

import T3.Web.Types

class Monad m => Web m where
  play :: Request -> m Response
  start :: Request -> m Response
  randomHandler :: Request -> m Response
  register :: Request -> m Response
  match :: Request -> m Response

class FromRequest a where
  fromRequest :: Request -> Maybe a

class ToResponse a where
  toResponse :: a -> Response
