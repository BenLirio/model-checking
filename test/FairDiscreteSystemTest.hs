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

instance FOL.Relatable Predicate where
  relation Eq = Just FOL.Equal
  relation Lt = Just FOL.LessThan

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
          (FOL.Predicate Eq
            [ FOL.Variable (FDS.Normal X)
            , FOL.Constant 0
            ])
          (FOL.Predicate Eq
            [ FOL.Variable (FDS.Prime X)
            , FOL.Function Add 
              [ FOL.Variable (FDS.Normal X)
              , FOL.Constant 3
              ]
            ]))
          :: Formula)
      `shouldBe`
      "((= X 0) ∧ (= X' (+ X 3)))"
    it "Should unprime Formula" $ do
      (FDS.unprime (FOL.Predicate Eq
        [ FOL.Variable (FDS.Prime Pc)
        , FOL.Function Add 
          [ FOL.Variable (FDS.Prime X)
          , FOL.Constant 3
          ]
        ]))
      `shouldBe`
      Just (FOL.Predicate Eq
        [ FOL.Variable (FDS.Normal Pc)
        , FOL.Function Add 
          [ FOL.Variable (FDS.Normal X)
          , FOL.Constant 3
          ]
        ])
    it "Should fail prime Formula" $ do
      FDS.prime (FOL.Predicate Eq [ FOL.Variable (FDS.Prime Pc) ]) :: Maybe Formula
      `shouldBe`
      Nothing
    it "Should do `post(state, transition)`" $ do
      (FDS.post
        (FOL.Predicate Eq
          [ FOL.Variable (FDS.Normal X)
          , FOL.Constant 0
          ])
        (FOL.And
          (FOL.Predicate Eq
            [ FOL.Variable (FDS.Normal X)
            , FOL.Constant 0
            ])
          (FOL.Predicate Eq
            [ FOL.Variable (FDS.Prime X)
            , FOL.Constant 1
            ]))) :: Maybe Formula
      `shouldBe`
      (Just (FOL.Predicate Eq
        [ FOL.Variable (FDS.Normal X)
        , FOL.Constant 1
        ]))