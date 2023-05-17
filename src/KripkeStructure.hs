module KripkeStructure where

import Data.Set (Set)
import qualified Data.Set as Set

type Proposition = String
type Transition a = (a, a)

type Labeling a = a -> Set Proposition

type Relation a = Set (Transition a)
data KripkeStructure a = KripkeStructure
  { atomicPropositions :: Set Proposition
  , states :: Set a
  , initialStates :: Set a
  , relation :: Relation a
  , labeling :: Labeling a
  }