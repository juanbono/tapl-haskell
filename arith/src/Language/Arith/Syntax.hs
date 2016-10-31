module Language.Arith.Syntax
  (
    Term (..)
  , isNumericalVal
  , isVal
  ) where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
  deriving (Eq)

instance Show Term where
  show TmTrue = "true"
  show TmFalse = "false"
  show (TmIf cond trueBranch falseBranch) =
    "if " ++ "(" ++ show cond ++ ")"
    ++ " then "
    ++ "(" ++ show trueBranch ++ ")"
    ++ " else "
    ++ "(" ++ show falseBranch ++ ")"
  show TmZero = "0"
  show (TmSucc t) = "succ " ++ "(" ++ show t ++ ")"
  show (TmPred t) = "pred " ++ "(" ++ show t ++ ")"
  show (TmIsZero t) = "iszero " ++ "(" ++ show t ++ ")"

-- Funciones auxiliares
isNumericalVal :: Term -> Bool
isNumericalVal TmZero = True
isNumericalVal (TmSucc t) = isNumericalVal t
isNumericalVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericalVal t
