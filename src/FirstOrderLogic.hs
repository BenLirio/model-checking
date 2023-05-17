module FirstOrderLogic where

import Data.Set (Set)
import qualified Data.Set as Set

data Term symbol constant function =
    Variable symbol
  | Constant constant
  | Function function [Term symbol constant function]
  deriving (Eq, Ord)
instance (Show symbol, Show constant, Show function) => Show (Term symbol constant function) where
  show (Variable symbol) = show symbol
  show (Constant constant) = show constant
  show (Function function terms) = "(" ++ show function ++ " " ++ termsString ++ ")" where
    termsString = foldr1 (\term1 term2 -> term1 ++ " " ++ term2) (map show terms)

data Formula predicate symbol constant function =
    Atomic predicate [Term symbol constant function]
  | Not
    (Formula predicate symbol constant function)
  | And
    (Formula predicate symbol constant function)
    (Formula predicate symbol constant function)
  | Or
    (Formula predicate symbol constant function)
    (Formula predicate symbol constant function)
  | Implies
    (Formula predicate symbol constant function)
    (Formula predicate symbol constant function)
  | ForAll
    symbol
    (Formula predicate symbol constant function)
  | ThereExists
    symbol
    (Formula predicate symbol constant function)
  deriving (Eq, Ord)

instance (Show predicate, Show symbol, Show constant, Show function) => Show (Formula predicate symbol constant function) where
  show (Atomic predicate terms) = "(" ++ predicateString ++ " " ++ termsString ++ ")" where
    predicateString = show predicate
    termsString = foldr1 (\term1 term2 -> term1 ++ " " ++ term2) (map show terms)
  show (Not f) = "¬" ++ show f
  show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
  show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
  show (Implies f1 f2) = "(" ++ show f1 ++ " → " ++ show f2 ++ ")"
  show (ForAll v f) = "∀" ++ show v ++ ". " ++ show f
  show (ThereExists v f) = "∃" ++ show v ++ ". " ++ show f