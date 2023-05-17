module FairDiscreteSystemTest where

import Test.Hspec

spec :: Spec
spec = do
  describe "FairDiscreteSystem" $ do
    it "should have a test" $ do
      True `shouldBe` True