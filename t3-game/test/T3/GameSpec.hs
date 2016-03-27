{-# LANGUAGE OverloadedStrings #-}

module T3.GameSpec (spec) where

import T3.Game.Core
import Test.Hspec
import Data.Aeson hiding (Result)

spec :: Spec
spec = do
  describe "result" $ do
    let playBack :: [(Loc, XO)] -> Result
        playBack = result . foldl (\b (loc,xo) -> insertXO loc xo b) emptyBoard
    it "X should win horizontal" $ do
      shouldBe
        (playBack [(Loc 0 0, X), (Loc 1 0, X), (Loc 2 0, X)])
        (Winner X)
    it "O should win vertical" $ do
      shouldBe
        (playBack [(Loc 2 0, O), (Loc 2 1, O), (Loc 2 2, O)])
        (Winner O)
    it "X should win diagonal (0,0) -> (2,2)" $ do
      shouldBe
        (playBack [(Loc 0 0, X), (Loc 1 1, X), (Loc 2 2, X)])
        (Winner X)
    it "O should win diagonal (2,0) -> (0,2)" $ do
      shouldBe
        (playBack [(Loc 2 0, O), (Loc 1 1, O), (Loc 0 2, O)])
        (Winner O)
    it "should be Unfinished" $ do
      shouldBe
        (playBack [])
        Unfinished
    it "should be a Tie" $ do
      shouldBe
        (playBack
          [ (Loc 0 0, O), (Loc 1 0, O), (Loc 2 0, X)
          , (Loc 0 1, X), (Loc 1 1, X), (Loc 2 1, O)
          , (Loc 0 2, O), (Loc 1 2, X), (Loc 2 2, X) ])
        Tie
  describe "board json" $ do
    it "parse a cell with X, O, or ' '" $ do
      decode "\"x\"" `shouldBe` Just (Just X)
      decode "\"o\"" `shouldBe` Just (Just O)
      decode "\" \"" `shouldBe` Just (Nothing :: Maybe XO)
    it "parse an empty board" $ do
      decode (encode emptyBoard) `shouldBe` Just emptyBoard
