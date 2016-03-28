{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module T3.Client where

import Control.Monad.RWS
import Control.Monad.IO.Class

import T3.Server
import T3.Match
import T3.Game

import Data.Aeson
import Control.Lens
import Network.Wreq

mainDef :: IO ()
mainDef = do
  let addr = "http://localhost:3000"
  let regReq = RegisterRequest (UserName "botty1")
  r <- asJSON =<< post (addr `mappend` "/api/register") (toJSON regReq)
  let eReg = (r ^. responseBody) :: Either RegisterError RegisterResponse
  print eReg
