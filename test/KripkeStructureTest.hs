module KripkeStructureTest where

import Test.Hspec
import KripkeStructure (KripkeStructure(..))
import Data.Set (Set)
import qualified Data.Set as Set

stopLightKripkeStructure :: KripkeStructure Int
stopLightKripkeStructure = KripkeStructure
  { atomicPropositions = Set.fromList [ "green", "yellow", "red" ]
  , states = Set.fromList [1, 2, 3]
  , initialStates = Set.fromList [1]
  , relation = Set.fromList [(1, 2), (2, 3), (3, 1)]
  , labeling = \s -> case s of
      1 -> Set.fromList ["green"]
      2 -> Set.fromList ["yellow"]
      3 -> Set.fromList ["red"]
  }


spec :: Spec
spec = do
  describe "stopLightKripkeStructure" $ do
    it "should have three Predicate propositions" $ do
      length (atomicPropositions stopLightKripkeStructure) `shouldBe` 3
    it "should have three states" $ do
      length (states stopLightKripkeStructure) `shouldBe` 3
    it "should have one initial state" $ do
      length (initialStates stopLightKripkeStructure) `shouldBe` 1
    it "should have three transitions" $ do
      length (relation stopLightKripkeStructure) `shouldBe` 3