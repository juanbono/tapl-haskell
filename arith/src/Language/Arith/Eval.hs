module Language.Arith.Eval
  (
    singleStepEvaluator
  , bigStepEvaluator
  , eval
  ) where

import Language.Arith.Syntax

data Evaluator = SingleStepEval (Term -> Maybe Term)
               | BigStepEval (Term -> Maybe Term)

singleStepEvaluator :: Evaluator
singleStepEvaluator = SingleStepEval eval1

bigStepEvaluator :: Evaluator
bigStepEvaluator = BigStepEval bigEval

eval1 :: Term -> Maybe Term
-- los valores se evaluan a si mismos en 1 paso
eval1 t | isVal t = Just t

-- en el caso que la condicion no sea un valor, evalua 1 paso la condicion,
-- sino devuelve una de las ramas
eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) | not $ isVal t1 = (\t1' -> TmIf t1' t2 t3) <$> eval1 t1

-- si t no es un valor, evalua en 1 paso t
eval1 (TmSucc t) | not $ isVal t =  TmSucc <$> eval1 t

-- si t no es un valor (ni 0 , ni succ nv), evalua t
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc nv)) | isNumericalVal nv = Just nv
eval1 (TmPred t) | not $ isVal t = TmPred <$> eval1 t

-- si t no es un valor, evaluo t
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero t) | not $ isVal t = TmIsZero <$> eval1 t

-- si no aplica ninguno de los casos anteriores, el termino no se evalua.
eval1 _ = Nothing

bigEval :: Term -> Maybe Term
bigEval = undefined

eval :: Evaluator -> Term -> Maybe Term -- reduce a forma normal usando un evaluador, revisar tipo
eval = undefined

