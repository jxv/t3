module T3.Server.Match.ImplSpec (spec) where

import Test.Hspec
import Data.Aeson hiding (Result)

import T3.Server.Match ()
import T3.Server.Match.Impl

spec :: Spec
spec = do
  describe "_" $ do
    it "_" $ do
      True `shouldBe` False
