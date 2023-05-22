module FairDiscreteSystemTest where

import Test.Hspec
import Control.Exception (evaluate)
import qualified FairDiscreteSystem as FDS
import qualified FirstOrderLogic as FOL
import Text.ParserCombinators.ReadP

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
instance Read Predicate where
  readsPrec = const $ readP_to_S $ do
    p <- choice [string "=", string "<"]
    return $ case p of
      "=" -> Eq
      "<" -> Lt

data Function =
    Add
  deriving (Eq, Ord)
instance Show Function where
  show Add = "+"
instance Read Function where
  readsPrec = const $ readP_to_S $ do
    f <- string "+"
    return Add


data Variable =
    Pc
  | X
  deriving (Eq, Ord, Show)
instance Read Variable where
  readsPrec = const $ readP_to_S $ do
    v <- choice [string "Pc", string "X"]
    return $ case v of
      "Pc" -> Pc
      "X" -> X


readTest :: (Read a, Eq a, Show a) => String -> a -> Spec
readTest s x = it ("should read " ++ s) $ do
  read s `shouldBe` x


symbols :: Set Variable
symbols = Set.fromList [Pc, X]

symbolsWithPrimes :: Set (FDS.PrimedCopy Variable)
symbolsWithPrimes = Set.map FDS.Normal symbols `Set.union` Set.map FDS.Prime symbols

  
type Constant = Int

type Formula = FOL.Formula Predicate (FDS.PrimedCopy Variable) Constant Function
type Term = FOL.Term (FDS.PrimedCopy Variable) Constant Function

spec :: Spec
spec = do
  describe "Readers" $ do
    readTest "Pc" Pc
    readTest "X" X
    readTest "X'" $ FDS.Prime X
    readTest "=" Eq
    readTest "<" Lt
    readTest "X" $ (FOL.Variable (FDS.Normal X) :: Term)
    readTest "X'" $ (FOL.Variable (FDS.Prime X) :: Term)
    readTest "1" $ (FOL.Constant 1 :: Term)
    readTest "X" $ (X :: Variable)
    readTest "(+)" $ (FOL.Function Add [] :: Term)
    readTest "(+ 1)" $ (FOL.Function Add
      [ FOL.Constant 1
      ] :: Term)
    readTest "(+ X 1)" $ (FOL.Function Add
      [ FOL.Variable (FDS.Normal X)
      , FOL.Constant 1
      ] :: Term)
    readTest "(+ X X')" $ (FOL.Function Add
      [ FOL.Variable (FDS.Normal X)
      , FOL.Variable (FDS.Prime X)
      ] :: Term)
    readTest "(+ (+ X) 1)" $ (FOL.Function Add
      [ FOL.Function Add
        [ FOL.Variable (FDS.Normal X)
        ]
      , FOL.Constant 1
      ] :: Term)
    readTest "(+ (+ 1 2) (+ 3 4))" $ (FOL.Function Add
      [ FOL.Function Add
        [ FOL.Constant 1
        , FOL.Constant 2
        ]
      , FOL.Function Add
        [ FOL.Constant 3
        , FOL.Constant 4
        ]
      ] :: Term)
    readTest "(+ (+ 1 2) (+))" $ (FOL.Function Add
      [ FOL.Function Add
        [ FOL.Constant 1
        , FOL.Constant 2
        ]
      , FOL.Function Add []
      ] :: Term)
    readTest "(+ X (+) (+ 1 2))" $ (FOL.Function Add
      [ FOL.Variable (FDS.Normal X)
      , FOL.Function Add []
      , FOL.Function Add
        [ FOL.Constant 1
        , FOL.Constant 2
        ]
      ] :: Term)
    readTest "(+ (+))" $ (FOL.Function Add
      [ FOL.Function Add []
      ] :: Term)
    readTest "(+ X (+))" $ (FOL.Function Add
      [ FOL.Variable (FDS.Normal X)
      , FOL.Function Add []
      ] :: Term)
    readTest "(+ (+) (+))" $ (FOL.Function Add
      [ FOL.Function Add []
      , FOL.Function Add []
      ] :: Term)

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
    -- it "Should do `post(state, transition)`" $ do
    --   (FOL.simplify <$> ((FDS.post
    --     (FOL.Predicate Eq
    --       [ FOL.Variable (FDS.Normal X)
    --       , FOL.Constant 0
    --       ])
    --     (FOL.And
    --       (FOL.Predicate Eq
    --         [ FOL.Variable (FDS.Normal X)
    --         , FOL.Constant 0
    --         ])
    --       (FOL.Predicate Eq
    --         [ FOL.Variable (FDS.Prime X)
    --         , FOL.Constant 1
    --         ]))) :: Maybe Formula)) :: Maybe (FOL.SimplificationResult Formula)
    --   `shouldBe`
    --     (Just (FOL.Unknown (FOL.Predicate Eq
    --       [ FOL.Variable (FDS.Normal X)
    --       , FOL.Constant 1
    --       ])))
    -- it "Should calculate reachability point" $ do
    --   FDS.reachability (FDS.FairDiscreteSystem
    --     { FDS.variables = symbolsWithPrimes
    --     , FDS.initialCondition = FOL.And
    --         (FOL.Predicate Eq
    --         [ FOL.Variable (FDS.Normal Pc)
    --         , FOL.Constant 0
    --         ])
    --         (FOL.Predicate Eq
    --         [ FOL.Variable (FDS.Normal Pc)
    --         , FOL.Constant 0
    --         ])
    --     , FDS.transitionRelation = FOL.Or
    --       (FOL.And
    --         (FOL.And
    --           (FOL.Predicate Eq
    --             [ FOL.Variable (FDS.Normal Pc)
    --             , FOL.Constant 0
    --             ])
    --           (FOL.Predicate Eq
    --             [ FOL.Variable (FDS.Prime Pc)
    --             , FOL.Constant 1
    --             ]))
    --         (FOL.Predicate Eq
    --           [ FOL.Variable (FDS.Prime X)
    --           , FOL.Function Add 
    --             [ FOL.Variable (FDS.Normal X)
    --             , FOL.Constant 1
    --             ]]))
    --       (FOL.And
    --         (FOL.Predicate Eq
    --           [ FOL.Variable (FDS.Prime Pc)
    --           , FOL.Variable (FDS.Normal Pc)
    --           ])
    --         (FOL.Predicate Eq
    --           [ FOL.Variable (FDS.Prime X)
    --           , FOL.Variable (FDS.Normal X)
    --           ]))
    --     })
    --   `shouldBe`
    --   Just (FOL.Predicate Eq
    --     [ FOL.Variable (FDS.Normal Pc)
    --     , FOL.Constant 1
    --     ])