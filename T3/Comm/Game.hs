module T3.Comm.Game where

import Prelude
import Data.Text (Text)
import T3.Game
import T3.Comm.Class
import T3.Comm.Types

move' :: Comm m => (XO -> UserId) -> XO -> m (Maybe Loc)
move' userId xo = do
  let ui = userId xo
  sendGameState ui
  loc <- recvMove ui
  return $ Just loc

forfeit' :: Comm m => (XO -> UserId) -> Win XO -> Lose XO -> m ()
forfeit' userId (Win w) (Lose l) = do
  sendFinal (userId w) LossByDQ
  sendFinal (userId l) WonByDQ

end' :: Comm m => (XO -> UserId) -> Win XO -> Lose XO -> m ()
end' userId (Win w) (Lose l) = do
  sendFinal (userId w) Won
  sendFinal (userId l) Loss

tie' :: Comm m => (XO -> UserId) -> m ()
tie' userId = do
  sendFinal (userId X) Tied
  sendFinal (userId O) Tied

step' :: Comm m => Board -> m ()
step' b = do
  updateBoard b

invalidMove' :: Comm m => (XO -> UserId) -> XO -> m ()
invalidMove' userId xo = do
  ackInvalidMove (userId xo)

{-
import Prelude

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Control.Monad
import Data.Int
import Data.Aeson

parseXO :: Char -> Maybe XO
parseXO 'X' = Just X
parseXO 'x' = Just X
parseXO 'O' = Just O
parseXO 'o' = Just O
parseXO _ = Nothing

instance FromJSON XO where
  parseJSON (String s) =
    if T.null s
    then mzero
    else maybe mzero pure (parseXO $ T.head s)
  parseJSON _ = mzero


data Player = Player {
  pId :: Int64,
  pName :: Text,
  pXO :: XO,
  pStatus :: Maybe Text
} deriving (Show, Eq)

instance FromJSON Player where
  parseJSON (Object o) = Player
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "xo"
    <*> o .: "status"
  parseJSON _ = mzero

data Game = Game {
  gId :: Int64,
  gPlayers :: [Player],
  gBoard :: Board
} deriving (Show, Eq)

instance FromJSON Game where
  parseJSON (Object o) = Game
    <$> o .: "id"
    <*> o .: "players"
    <*> o .: "board"
  parseJSON _ = mzero

data UserInit = UserInit {
  uiKey :: Text
} deriving (Show, Eq)

instance FromJSON UserInit where
  parseJSON (Object o) = UserInit
    <$> o .: "key"
  parseJSON _ = mzero

data Init = Init {
  iGame :: Game,
  iToken :: Text,
  iPlayUrl :: Text
} deriving (Show, Eq)

instance FromJSON Init where
  parseJSON (Object o) = Init
    <$> o .: "game"
    <*> o .: "token"
    <*> o .: "playUrl"
  parseJSON _ = mzero

data Step = Step {
  sGame :: Game,
  sFinished :: Bool
} deriving (Show, Eq)

instance FromJSON Step where
  parseJSON (Object o) = Step
    <$> o .: "game"
    <*> o .: "finished"
  parseJSON _ = mzero

data Move = Move {
  mKey :: Text,
  mLoc :: (Int, Int)
} deriving (Show, Eq)
-}
