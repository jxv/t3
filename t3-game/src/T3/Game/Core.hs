{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T3.Game.Core
  ( XO(..)
  , Loc(..)
  , Action(..)
  , Result(..)
  , Board
  , yinYang
  , emptyBoard
  , boardMap
  , boardList
  , boardSize
  , insertXO
  , inside
  , valid
  , result
  , dropPrefixP
  , dropPrefixJ
  ) where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import GHC.Generics
import Control.Monad (mzero)
import Data.Aeson hiding (Result)
import Data.Aeson.Types hiding (Result)
import Data.Char (toLower)
import Safe.Exact
import Data.Maybe

import Debug.Trace

data XO
  = X
  | O
  deriving (Show, Eq, Generic, ToJSON)

data Loc = Loc
  { locX :: Int
  , locY :: Int
  } deriving (Show, Read, Eq, Ord, Generic)

data Action = Action
  { actXO :: XO
  , actLoc :: Loc
  } deriving (Show, Eq, Generic)

data Board = Board
  { bCells :: M.Map Loc XO
  , bSize :: Int
  } deriving (Show, Eq)

data Result
  = Unfinished
  | Tie
  | Winner XO
  deriving (Show, Eq)

yinYang :: XO -> XO
yinYang X = O
yinYang O = X

emptyBoard :: Board
emptyBoard = Board M.empty 3

boardMap :: Board -> M.Map Loc XO
boardMap = bCells

boardList :: Board -> [Maybe XO]
boardList b = [M.lookup (Loc x y) (bCells b) | y <- q, x <- q]
  where q = indices b

boardSize :: Board -> Int
boardSize = bSize

inside :: Loc -> Board -> Bool
inside loc b = x >= 0 && x < bSize b && y >= 0 && y < bSize b
  where
    x = locX loc
    y = locY loc

valid :: Loc -> Board -> Bool
valid loc b = inside loc b && not (M.member loc (bCells b))

insertXO :: Loc -> XO -> Board -> Board
insertXO loc xo b =
  if inside loc b
  then b { bCells = M.insert loc xo (bCells b) }
  else b

result :: Board -> Result
result b
  | isWinner X b = Winner X
  | isWinner O b = Winner O
  | M.size (bCells b) == (bSize b) ^ (2 :: Int) = Tie
  | otherwise = Unfinished

isWinner :: XO -> Board -> Bool
isWinner xo b =
  or [all has [Loc x y | y <- q] | x <- q] ||
  or [all has [Loc x y | x <- q] | y <- q] ||
  all has [Loc z z | z <- q] ||
  all has [Loc z (bSize b - 1 - z) | z <- q]
  where
    has loc = M.lookup loc (bCells b) == Just xo
    q = indices b

indices :: Board -> [Int]
indices b = [0..bSize b - 1]

instance FromJSON Loc where
  parseJSON (Object o) = Loc <$> o .: "x" <*> o .: "y"
  parseJSON _ = mzero

instance FromJSON XO where
  parseJSON (String xo)
    | xo' == "x" = pure X
    | xo' == "o" = pure O
    | otherwise = mzero
    where
      xo' = T.map toLower xo
  parseJSON _ = mzero

instance FromJSON (Maybe XO) where
  parseJSON o@(String s) = if s == " " then pure Nothing  else fmap Just (parseJSON o)
  parseJSON _ = mzero

instance FromJSON Board where
  parseJSON b = Board <$> (M.fromList <$> board b) <*> pure size
    where
      size = 3
      board o = do
        cells :: [[Maybe XO]] <- parseJSON o
        let correctRowSize = length cells == size
        let correctColSize = and $ map ((== size) . length) cells
        let pairs = [ (Loc x y, fromJust cell) | y <- [0..pred size], x <- [0..pred size], let cell = cells !! y !! x, isJust cell ]
        if correctRowSize && correctColSize then return pairs else mzero

instance ToJSON Board where
  toJSON b = toJSON [[cvt $ M.lookup (Loc x y) m | x <- [0..pred s]] | y <- [0..pred s]]
    where
      m = boardMap b
      s = boardSize b
      cvt :: Maybe XO -> Value
      cvt = maybe (String " ") toJSON

instance ToJSON Loc where
  toJSON = dropPrefixJ "loc"

instance ToJSON Action where
  toJSON = dropPrefixJ "act"

-- dropPrefixP :: (Generic a, GFromJSON (Rep a)) => String -> Value -> Parser a
dropPrefixP prefix = genericParseJSON defaultOptions { fieldLabelModifier = dropPrefix prefix }

-- dropPrefixJ :: (Generic a, GToJSON (Rep a)) => String -> a -> Value
dropPrefixJ prefix = genericToJSON defaultOptions { fieldLabelModifier = dropPrefix prefix }

dropPrefix :: String -> String -> String
dropPrefix prefix = (\(c:cs) -> toLower c : cs) . drop (length prefix)
