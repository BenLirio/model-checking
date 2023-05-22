module FirstOrderLogic where

import Data.Set (Set)
import qualified Data.Set as Set

import Text.ParserCombinators.ReadP


data Term variable constant function =
    Variable variable
  | Constant constant
  | Function function [Term variable constant function]
  deriving (Eq, Ord)

instance (Show variable, Show constant, Show function) => Show (Term variable constant function) where
  show (Variable variable) = show variable
  show (Constant constant) = show constant
  show (Function function []) = "(" ++ show function ++ ")"
  show (Function function terms) = "(" ++ show function ++ " " ++ termsString ++ ")"
    where
    termsString = unwords $ map show terms


instance (Read v, Read c, Read f) => Read (Term v c f) where
  readsPrec = const $ readP_to_S $ do
    t <- choice [variable, constant, function]
    return t
    where
      variable = do
        v <- readS_to_P reads
        return $ Variable v
      constant = do
        c <- readS_to_P reads
        return $ Constant c
      function = do
        char '('
        f <- readS_to_P reads
        skipSpaces
        terms <- sepBy (choice [variable, constant, function]) (char ' ')
        char ')'
        return $ Function f terms




      


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

(<||>) :: Maybe a -> Maybe a -> Maybe a
Just x <||> Nothing = Just x
Nothing <||> Just x = Just x
Nothing <||> Nothing = Nothing
Just _ <||> Just _ = Nothing




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
    And f1 f2 -> getConstantEqualityOfVariableInFormula f1 x <||> getConstantEqualityOfVariableInFormula f2 x
    Or f1 f2 -> getConstantEqualityOfVariableInFormula f1 x <||> getConstantEqualityOfVariableInFormula f2 x
    Implies f1 f2 -> getConstantEqualityOfVariableInFormula f1 x <||> getConstantEqualityOfVariableInFormula f2 x
    _ -> Nothing

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

data SimplificationResult a =
    Tautology
  | Contradiction
  | Unknown a
  deriving (Eq, Ord)
instance (Show a) => Show (SimplificationResult a) where
  show Tautology = "True"
  show Contradiction = "False"
  show (Unknown a) = show a

simplify :: (Relatable p, Eq v, Eq c) => Formula p v c f -> SimplificationResult (Formula p v c f)
simplify formula = case formula of
  Predicate p terms -> case relation p of
    Just Equal -> case terms of
      [Variable v1, Variable v2] -> if v1 == v2 then Tautology else Unknown formula
      [Constant c1, Constant c2] -> if c1 == c2 then Tautology else Contradiction
      _ -> Unknown formula
    _ -> Unknown formula
  Not f -> case simplify f of
    Tautology -> Contradiction
    Contradiction -> Tautology
    Unknown f' -> Unknown (Not f')
  And f1 f2 -> case (simplify f1, simplify f2) of
    (Contradiction, _) -> Contradiction
    (_, Contradiction) -> Contradiction
    (Tautology, Unknown f2') -> Unknown f2'
    (Unknown f1', Tautology) -> Unknown f1'
    (Tautology, Tautology) -> Tautology
  Or f1 f2 -> simplify (Not (And (Not f1) (Not f2)))
  Implies f1 f2 -> simplify (Or (Not f1) f2)
  _ -> Unknown formula





data Relation =
    Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
class Relatable r where
  relation :: r -> Maybe Relation