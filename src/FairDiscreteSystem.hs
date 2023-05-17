module FairDiscreteSystem where

import qualified FirstOrderLogic

import Data.Set (Set)
import qualified Data.Set as Set

data WithPrimedSymbols symbol = 
    Normal symbol
  | Prime symbol
  deriving (Eq, Ord)

data FairDiscreteSystem predicate symbol constant function = FairDiscreteSystem
  { variables :: Set (WithPrimedSymbols symbol)
  , initialCondition :: FirstOrderLogic.Formula predicate (WithPrimedSymbols symbol) constant function
  , transitionRelation :: FirstOrderLogic.Formula predicate (WithPrimedSymbols symbol) constant function
  }
