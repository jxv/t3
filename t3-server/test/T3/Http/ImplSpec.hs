module T3.Http.ImplSpec (spec) where

import Test.Hspec
import Data.Aeson hiding (Result)

import T3.Http (Request(..), Response(..))
import T3.Http.Impl

spec :: Spec
spec = do
  describe "_" $ do
    it "_" $ do
      True `shouldBe` False
