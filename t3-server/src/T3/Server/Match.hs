{-# LANGUAGE DeriveGeneric #-}
module T3.Server.Match
  ( Match(..)
  ) where

import GHC.Generics
import Data.Char (toLower)
import Data.Aeson hiding (Result)
import Data.Aeson.Types hiding (Result)
import Data.Text (Text)

import T3.Core (XO, Loc, Board, Result, Action(..), dropPrefixP, dropPrefixJ)
import T3.Server (Final)
import T3.Game -- types

class Monad m => Match m where
  sendGameState :: XO -> m ()
  recvAction :: XO -> m Loc
  sendFinal :: XO -> Final -> m ()
  tally :: Result -> m ()
  updateBoard :: Board -> m ()
  logAction :: XO -> Loc -> m ()
