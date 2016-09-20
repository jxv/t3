module T3.Server.Types
  ( UserId(..)
  , Token(..)
  , Ticket(..)
  , GameId(..)
  , Move(..)
  , Step(..)
  ) where

import Data.Text (Text)
import Data.String (IsString)

newtype UserId = UserId Text
  deriving (Show, Eq, IsString)

newtype Token = Token Text
  deriving (Show, Eq, IsString)

newtype Ticket = Ticket Text
  deriving (Show, Eq, IsString)

newtype GameId = GameId Text
  deriving (Show, Eq, IsString)
 
newtype Move = Move (Int,Int)
  deriving (Show, Eq)

newtype Step = Step ()
  deriving (Show, Eq)
