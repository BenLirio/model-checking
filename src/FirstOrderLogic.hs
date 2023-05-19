module FirstOrderLogic where

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Applicative ((<|>))

data Term variable constant function =
    Variable variable
  | Constant constant
  | Function function [Term variable constant function]
  deriving (Eq, Ord)
instance (Show variable, Show constant, Show function) => Show (Term variable constant function) where
  show (Variable variable) = show variable
  show (Constant constant) = show constant
  show (Function function terms) = "(" ++ show function ++ " " ++ termsString ++ ")" where
    termsString = foldr1 (\term1 term2 -> term1 ++ " " ++ term2) (map show terms)

data Formula predicate variable constant function =
    Predicate predicate [Term variable constant function]
  | Not
    (Formula predicate variable constant function)
  | And
    (Formula predicate variable constant function)
    (Formula predicate variable constant function)
  | Or
    (Formula predicate variable constant function)
    (Formula predicate variable constant function)
  | Implies
    (Formula predicate variable constant function)
    (Formula predicate variable constant function)
  | ForAll
    variable
    (Formula predicate variable constant function)
  | ThereExists
    variable
    (Formula predicate variable constant function)
  deriving (Eq, Ord)

instance (Show predicate, Show variable, Show constant, Show function) => Show (Formula predicate variable constant function) where
  show (Predicate predicate terms) = "(" ++ predicateString ++ " " ++ termsString ++ ")" where
    predicateString = show predicate
    termsString = foldr1 (\term1 term2 -> term1 ++ " " ++ term2) (map show terms)
  show (Not f) = "¬" ++ show f
  show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
  show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
  show (Implies f1 f2) = "(" ++ show f1 ++ " → " ++ show f2 ++ ")"
  show (ForAll v f) = "∀" ++ show v ++ ". " ++ show f
  show (ThereExists v f) = "∃" ++ show v ++ ". " ++ show f


getConstantEqualityOfVariableInFormula :: (Relatable p, Eq v) => Formula p v c f -> v -> Maybe c
getConstantEqualityOfVariableInFormula formula x =
  case formula of
    Predicate p terms -> relation p >>= \r -> case r of
      Equal -> case terms of
        [Variable x', Constant c] -> if x' == x then Just c else Nothing
        [Constant c, Variable x'] -> if x' == x then Just c else Nothing
        _ -> Nothing
      _ -> Nothing
    Not f -> getConstantEqualityOfVariableInFormula f x
    And f1 f2 -> getConstantEqualityOfVariableInFormula f1 x <|> getConstantEqualityOfVariableInFormula f2 x

replaceVariableInTerm :: (Eq v) => v -> Term v c f -> Term v c f -> Term v c f
replaceVariableInTerm x x' term = case term of
  Variable y -> if x == y then x' else term
  Constant c -> term
  Function f terms -> Function f (map (replaceVariableInTerm x x') terms)


replaceVariableInFormula :: (Eq v) =>  v -> Term v c f -> Formula p v c f -> Formula p v c f
replaceVariableInFormula x x' formula = case formula of
  Predicate p terms -> Predicate p (map (replaceVariableInTerm x x') terms)
  Not f -> Not (replaceVariableInFormula x x' f)
  And f1 f2 -> And (replaceVariableInFormula x x' f1) (replaceVariableInFormula x x' f2)
  Or f1 f2 -> Or (replaceVariableInFormula x x' f1) (replaceVariableInFormula x x' f2)
  Implies f1 f2 -> Implies (replaceVariableInFormula x x' f1) (replaceVariableInFormula x x' f2)
  ForAll y f -> if x == y then formula else ForAll y (replaceVariableInFormula x x' f)
  ThereExists y f -> if x == y then formula else ThereExists y (replaceVariableInFormula x x' f)



data Relation =
    Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
class Relatable r where
  relation :: r -> Maybe Relation