module Eval
  (
    eval1
  ) where

import Syntax

isNumericalVal :: Term -> Bool
isNumericalVal TmZero = True
isNumericalVal (TmSucc t) = isNumericalVal t
isNumericalVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericalVal t

eval1 :: Term -> Term
eval1 (TmIf TmTrue t2 _) = t2
eval1 (TmIf TmFalse _ t3) = t3
eval1 (TmIf t1 t2 t3) = TmIf (eval1 t1) t2 t3
eval1 (TmSucc t) = TmSucc (eval1 t)
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred (TmSucc nv)) = if isNumericalVal nv then nv else undefined
eval1 (TmPred t) = TmPred (eval1 t)
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero (TmSucc _)) = TmFalse
