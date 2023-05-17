module FairDiscreteSystem where

import PropositionalLogic (Formula)

import Data.Set (Set)
import qualified Data.Set as Set


data FairDiscreteSystem a = FairDiscreteSystem
  { variables :: Set a
  , initialCondition :: Formula
  , transitionRelation :: Formula
  , justiceRequirements :: Set Formula
  , compassionRequirements :: Set (Formula, Formula)
  }