module PropositionalLogic where

type Proposition = String

data Formula =
    Atomic Proposition
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  | Iff Formula Formula