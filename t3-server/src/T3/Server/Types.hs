module T3.Server.Types
  ( UserId(..)
  , Name(..)
  , Token(..)
  , Ticket(..)
  , GameId(..)
  , Move(..)
  , Step(..)
  , RegistryCb(..)
  , LobbyCb(..)
  , UsherCb(..)
  , GamesCb(..)
  , ResultsCb(..)
  ) where

import Control.Lens
import Data.Text (Text)
import Data.String (IsString)

newtype UserId = UserId Text
  deriving (Show, Eq, IsString)

newtype Name = Name Text
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

data RegistryCb m = RegistryCb
  { _registryCbInsertUser :: Name -> Token -> m (Maybe UserId)
  }

data LobbyCb m = LobbyCb (m ())

data UsherCb m = UsherCb (m ())

data GamesCb m = GamesCb (m ())

data ResultsCb m = ResultsCb (m ())
