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


unprimeTerm :: FOL.Term (PrimedCopy s) c f -> FOL.Term (PrimedCopy s) c f
unprimeTerm term = 
  case term of
    FOL.Variable (Prime s) -> FOL.Variable (Normal s)
    FOL.Variable (Normal s) -> error "unprimeTerm: variable is not primed"
    FOL.Constant c -> FOL.Constant c
    FOL.Function f terms -> FOL.Function f (map unprimeTerm terms)

unprimeFormula :: FOL.Formula p (PrimedCopy s) c f -> FOL.Formula p (PrimedCopy s) c f
unprimeFormula formula =
  case formula of
    FOL.Atomic p terms -> FOL.Atomic p (map unprimeTerm terms)
    FOL.Not f -> unaryOp FOL.Not f
    FOL.And f1 f2 -> binaryOp FOL.And f1 f2
    FOL.Or f1 f2 -> binaryOp FOL.Or f1 f2
    FOL.Implies f1 f2 -> binaryOp FOL.Implies f1 f2
    FOL.ForAll s f -> FOL.ForAll s (unprimeFormula f)
    FOL.ThereExists s f -> FOL.ThereExists s (unprimeFormula f)
  where
    unaryOp constructor f = constructor (unprimeFormula f)
    binaryOp constructor f1 f2 = constructor (unprimeFormula f1) (unprimeFormula f2)


primeTerm :: FOL.Term (PrimedCopy s) c f -> FOL.Term (PrimedCopy s) c f
primeTerm term = 
  case term of
    FOL.Variable (Normal s) -> FOL.Variable (Prime s)
    FOL.Variable (Prime s) -> error "primeTerm: variable is already primed"
    FOL.Constant c -> FOL.Constant c
    FOL.Function f terms -> FOL.Function f (map primeTerm terms)

primeFormula :: FOL.Formula p (PrimedCopy s) c f -> FOL.Formula p (PrimedCopy s) c f
primeFormula formula =
  case formula of
    FOL.Atomic p terms -> FOL.Atomic p (map primeTerm terms)
    FOL.Not f -> unaryOp FOL.Not f
    FOL.And f1 f2 -> binaryOp FOL.And f1 f2
    FOL.Or f1 f2 -> binaryOp FOL.Or f1 f2
    FOL.Implies f1 f2 -> binaryOp FOL.Implies f1 f2
    FOL.ForAll s f -> FOL.ForAll s (primeFormula f)
    FOL.ThereExists s f -> FOL.ThereExists s (primeFormula f)
  where
    unaryOp constructor f = constructor (primeFormula f)
    binaryOp constructor f1 f2 = constructor (primeFormula f1) (primeFormula f2)

