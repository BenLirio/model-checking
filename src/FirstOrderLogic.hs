module FirstOrderLogic where

import Data.Set (Set)
import qualified Data.Set as Set

data Term symbol constant function =
    Variable symbol
  | Constant constant
  | Function function [Term symbol constant function]

data Formula predicate variable constant function =
    Atomic predicate [Term variable constant function]
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