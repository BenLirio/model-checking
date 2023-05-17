module FairDiscreteSystemTest where

import Test.Hspec
import qualified FairDiscreteSystem as FDS
import qualified FirstOrderLogic as FOL

import Data.Set (Set)
import qualified Data.Set as Set


data Predicate =
    Eq
  | Lt
data Function =
    Add
data Symbol =
    Pc
  | X
  deriving (Eq, Ord)
symbols :: Set Symbol
symbols = Set.fromList [Pc, X]

  
type Constant = Int

simpleFDS :: FDS.FairDiscreteSystem Predicate Symbol Constant Function
simpleFDS = FDS.FairDiscreteSystem
  { FDS.variables = Set.fromList
    [ FDS.Normal Pc
    , FDS.Prime Pc
    , FDS.Normal X
    , FDS.Prime X
    ]
  , FDS.initialCondition = FOL.And
    (FOL.Atomic Eq [FOL.Variable (FDS.Normal Pc), FOL.Constant 0])
    (FOL.Atomic Eq [FOL.Variable (FDS.Normal X), FOL.Constant 0])
  , FDS.transitionRelation = FOL.Or
    (FOL.And
      (FOL.And
        (FOL.Atomic Lt [FOL.Variable (FDS.Normal Pc), FOL.Constant 3])
        (FOL.Atomic Eq [FOL.Variable (FDS.Prime Pc), FOL.Function Add [FOL.Variable (FDS.Normal Pc), FOL.Constant 1] ])
      )
      (FOL.Atomic Eq [FOL.Variable (FDS.Prime X), FOL.Function Add [FOL.Variable (FDS.Normal X), FOL.Constant 3] ])
    )
    (FOL.And
      (FOL.And
        (FOL.Atomic Eq [FOL.Variable (FDS.Normal Pc), FOL.Constant 3])
        (FOL.Atomic Eq [FOL.Variable (FDS.Prime Pc), FOL.Constant 3])
      )
      (FOL.Atomic Eq [FOL.Variable (FDS.Prime X), FOL.Variable (FDS.Normal X)])
    )

  }



spec :: Spec
spec = do
  describe "FairDiscreteSystem" $ do
    it "should have a test" $ do
      True `shouldBe` True