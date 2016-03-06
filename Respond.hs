module Respond (responder) where

import Import
import T3.Session.Types
import T3.Web

responder :: Web w => w Resp -> HandlerT App w ()
responder sr = do
  _ <- lift sr
  return ()
