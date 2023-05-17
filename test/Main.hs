import qualified FairDiscreteSystemTest
import qualified KripkeStructureTest

import Test.Hspec
import FairDiscreteSystem (FairDiscreteSystem(..))

main :: IO ()
main = hspec $ do
  describe "FairDiscreteSystem" $ do
    FairDiscreteSystemTest.spec
  describe "KripkeStructure" $ do
    KripkeStructureTest.spec