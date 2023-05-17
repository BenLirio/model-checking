module FairDiscreteSystemTest where

import Test.Hspec
import Control.Exception (evaluate)
import qualified FairDiscreteSystem as FDS
import qualified FirstOrderLogic as FOL

import Data.Set (Set)
import qualified Data.Set as Set


data Predicate =
    Eq
  | Lt
  deriving (Eq, Ord)

instance Show Predicate where
  show Eq = "="
  show Lt = "<"

data Function =
    Add
  deriving (Eq, Ord)
instance Show Function where
  show Add = "+"

data Symbol =
    Pc
  | X
  deriving (Eq, Ord, Show)

symbols :: Set Symbol
symbols = Set.fromList [Pc, X]

  
type Constant = Int

type Formula = FOL.Formula Predicate (FDS.PrimedCopy Symbol) Constant Function
type Term = FOL.Term (FDS.PrimedCopy Symbol) Constant Function

spec :: Spec
spec = do
  describe "Formula" $ do
    it "should show" $ do
      show
        ((FOL.And
          (FOL.Atomic Eq
            [ FOL.Variable (FDS.Normal X)
            , FOL.Constant 0
            ])
          (FOL.Atomic Eq
            [ FOL.Variable (FDS.Prime X)
            , FOL.Function Add 
              [ FOL.Variable (FDS.Normal X)
              , FOL.Constant 3
              ]
            ]))
          :: Formula)
      `shouldBe`
      "((= X 0) ∧ (= X' (+ X 3)))"
    it "Should unprime Term" $ do
      (FDS.unprimeTerm ((FOL.Variable (FDS.Prime X)) :: Term))
      `shouldBe`
      (FOL.Variable (FDS.Normal X))
    it "Should fail to unprime Term" $ do
      evaluate (FDS.unprimeTerm ((FOL.Variable (FDS.Normal X)) :: Term))
      `shouldThrow`
      errorCall "unprimeTerm: variable is not primed"
    it "Should unprime Formula" $ do
      (FDS.unprimeFormula (FOL.Atomic Eq
        [ FOL.Variable (FDS.Prime Pc)
        , FOL.Function Add 
          [ FOL.Variable (FDS.Prime X)
          , FOL.Constant 3
          ]
        ]))
      `shouldBe`
      (FOL.Atomic Eq
        [ FOL.Variable (FDS.Normal Pc)
        , FOL.Function Add 
          [ FOL.Variable (FDS.Normal X)
          , FOL.Constant 3
          ]
        ])
    it "Should prime Term" $ do
      (FDS.primeTerm (FOL.Function Add
        [ FOL.Variable (FDS.Normal X)
        , FOL.Constant 3
        ]))
      `shouldBe`
      (FOL.Function Add
        [ FOL.Variable (FDS.Prime X)
        , FOL.Constant 3
        ])
    it "Should fail to prime Formula" $
      evaluate (FDS.primeFormula (FOL.Atomic Eq
        [ FOL.Variable (FDS.Prime Pc)
        , FOL.Function Add 
          [ FOL.Variable (FDS.Prime X)
          , FOL.Constant 3
          ]
        ]))
      `shouldThrow`
      errorCall "primeFormula: variable is already primed"




  describe "FairDiscreteSystem" $ do
    it "should have a test" $ do
      True `shouldBe` True
    it "checks errors" $ do
      evaluate (error "foo") `shouldThrow` errorCall "foo"