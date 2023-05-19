module FairDiscreteSystem where

import qualified FirstOrderLogic as FOL

import Data.Set (Set)
import qualified Data.Set as Set

data PrimedCopy s =
    Prime s
  | Normal s
  deriving (Eq, Ord)
instance (Show s) => Show (PrimedCopy s) where
  show (Prime s) = show s ++ "'"
  show (Normal s) = show s


data FairDiscreteSystem predicate symbol constant function = FairDiscreteSystem
  { variables :: Set (PrimedCopy symbol)
  , initialCondition :: FOL.Formula predicate (PrimedCopy symbol) constant function
  , transitionRelation :: FOL.Formula predicate (PrimedCopy symbol) constant function
  }

type MapPrimedCopy v = (PrimedCopy v) -> Maybe (PrimedCopy v)


mapVariableInTerm :: MapPrimedCopy v -> FOL.Term (PrimedCopy v) c f -> Maybe (FOL.Term (PrimedCopy v) c f)
mapVariableInTerm mapFunc term = case term of
  FOL.Variable x ->
    mapFunc x >>= \x' ->
    return $ FOL.Variable x'
  FOL.Constant c -> return term
  FOL.Function f terms ->
    sequence (map (mapVariableInTerm mapFunc) terms) >>= \terms' ->
    return $ FOL.Function f terms'

mapVariableInFormula :: MapPrimedCopy v -> FOL.Formula p (PrimedCopy v) c f -> Maybe (FOL.Formula p (PrimedCopy v) c f)
mapVariableInFormula mapFunc formula = case formula of
  FOL.Predicate p terms ->
    sequence (map (mapVariableInTerm mapFunc) terms) >>= \terms' ->
    return $ FOL.Predicate p terms'
  FOL.Not f ->
    mapVariableInFormula mapFunc f >>= \f' ->
    return $ FOL.Not f'
  FOL.And f1 f2 ->
    mapVariableInFormula mapFunc f1 >>= \f1' ->
    mapVariableInFormula mapFunc f2 >>= \f2' ->
    return $ FOL.And f1' f2'
  FOL.Or f1 f2 ->
    mapVariableInFormula mapFunc f1 >>= \f1' ->
    mapVariableInFormula mapFunc f2 >>= \f2' ->
    return $ FOL.Or f1' f2'
  FOL.Implies f1 f2 ->
    mapVariableInFormula mapFunc f1 >>= \f1' ->
    mapVariableInFormula mapFunc f2 >>= \f2' ->
    return $ FOL.Implies f1' f2'
  FOL.ForAll x f ->
    mapVariableInFormula mapFunc f >>= \f' ->
    mapFunc x >>= \x' ->
    return $ FOL.ForAll x' f'
  FOL.ThereExists x f ->
    mapVariableInFormula mapFunc f >>= \f' ->
    mapFunc x >>= \x' ->
    return $ FOL.ThereExists x' f'

unprimedVariablesInTerm :: (Ord v) => FOL.Term (PrimedCopy v) c f -> Set (PrimedCopy v)
unprimedVariablesInTerm term = case term of
  FOL.Variable (Normal x) -> Set.singleton (Normal x)
  FOL.Variable (Prime x) -> Set.empty
  FOL.Constant c -> Set.empty
  FOL.Function f terms -> Set.unions $ map unprimedVariablesInTerm terms
unprimedVariablesInFormula :: (Ord v) => FOL.Formula p (PrimedCopy v) c f -> Set (PrimedCopy v)
unprimedVariablesInFormula formula = case formula of
  FOL.Predicate _ terms -> Set.unions $ map unprimedVariablesInTerm terms
  FOL.Not f -> unprimedVariablesInFormula f
  FOL.And f1 f2 -> Set.union (unprimedVariablesInFormula f1) (unprimedVariablesInFormula f2)
  FOL.Or f1 f2 -> Set.union (unprimedVariablesInFormula f1) (unprimedVariablesInFormula f2)
  FOL.Implies f1 f2 -> Set.union (unprimedVariablesInFormula f1) (unprimedVariablesInFormula f2)
  FOL.ForAll x f -> Set.insert x (unprimedVariablesInFormula f)
  FOL.ThereExists x f -> Set.insert x (unprimedVariablesInFormula f)


unprime :: FOL.Formula p (PrimedCopy v) c f -> Maybe (FOL.Formula p (PrimedCopy v) c f)
unprime = mapVariableInFormula unprime'
  where
    unprime' (Prime x) = return $ Normal x
    unprime' (Normal x) = Nothing
prime :: FOL.Formula p (PrimedCopy v) c f -> Maybe (FOL.Formula p (PrimedCopy v) c f)
prime = mapVariableInFormula prime'
  where
    prime' (Prime x) = Nothing
    prime' (Normal x) = Just $ Prime x

post :: (Ord v, FOL.Relatable p) => FOL.Formula p (PrimedCopy v) c f -> FOL.Formula p (PrimedCopy v) c f -> Maybe (FOL.Formula p (PrimedCopy v) c f)
post state delta =
  let primedPost = FOL.And state delta in
  let unprimedVariables = Set.toList $ unprimedVariablesInFormula primedPost in

  (sequence (map (FOL.getConstantEqualityOfVariableInFormula primedPost) unprimedVariables) >>= \constants ->
  let constantTerms = map FOL.Constant constants in
  unprime (foldl (\formula replacementFunc -> replacementFunc formula) primedPost (zipWith FOL.replaceVariableInFormula unprimedVariables constantTerms)) >>= \unprimedPost ->
  return unprimedPost)
  
  
  