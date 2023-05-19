import KripkeStructure (KripkeStructure(..))
import Data.Set (Set)
import qualified Data.Set as Set

kripkeStructure :: KripkeStructure Int
kripkeStructure = KripkeStructure
  { AtomicPropositions = Set.fromList ["p", "q"]
  , states = Set.fromList [1, 2, 3]
  , initialStates = Set.fromList [1]
  , relation = Set.fromList [(1, 2), (2, 3), (3, 3)]
  , labeling = \s -> case s of
      1 -> Set.fromList ["p"]
      2 -> Set.fromList ["q"]
      3 -> Set.fromList ["p", "q"]
  }

main :: IO ()
main = print "foo"