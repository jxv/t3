module Test.T3.Server.LobbyImplSpec (spec) where

import Test.Hspec

import T3.Server.Lobby hiding (Lobby(..))

spec :: Spec
spec = do
  describe "enter lobby" $ do
    it "asdf" $ do
      True `shouldBe` True
