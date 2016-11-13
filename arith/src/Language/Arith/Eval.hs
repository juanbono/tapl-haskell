module Language.Arith.Eval
  (
    small
  , multi
  , big
  ) where

import Language.Arith.Syntax
import Control.Monad ((<=<))

-- | small documentation
small :: Term -> Maybe Term
small t | isVal t = Just t

-- E-IfTrue
small (TmIf TmTrue t2 _) = Just t2
-- E-IfFalse
small (TmIf TmFalse _ t3) = Just t3
-- E-If
small (TmIf t1 t2 t3) | not $ isVal t1 = (\t1' -> TmIf t1' t2 t3) <$> small t1

-- E-Succ
small (TmSucc t) | not $ isVal t =  TmSucc <$> small t

-- E-PredZero
small (TmPred TmZero) = Just TmZero
-- E-PredSucc
small (TmPred (TmSucc nv)) | isNumericalVal nv = Just nv
-- E-Pred
small (TmPred t) | not $ isVal t = TmPred <$> small t

-- E-IsZeroZero
small (TmIsZero TmZero) = Just TmTrue
-- E-IsZeroSucc
small (TmIsZero (TmSucc nv)) | isNumericalVal nv = Just TmFalse
-- E-IsZero
small (TmIsZero t) | not $ isVal t = TmIsZero <$> small t

small _ = Nothing

-- | multi documentation
multi :: Term -> Maybe Term
multi t
  = case small t of
      Just t' -> if isVal t'
                 then return t'
                 else multi t'
      Nothing -> Nothing

multi2 :: Term -> Maybe Term
multi2  = small <=< small

-- | big documentation
big :: Term -> Maybe Term
-- B-IfTrue and B-IfFalse
big (TmIf t1 t2 t3)
  = case big t1 of
      Just TmTrue  -> big t2
      Just TmFalse -> big t3
      _            -> Nothing

-- B-Succ
big (TmSucc t1)
  = do nv1 <- big t1
       if isNumericalVal nv1
         then return $ TmSucc nv1
         else Nothing

-- B-PredZero and B-PredSucc
big (TmPred t1)
  = case big t1 of
      Just TmZero       -> Just TmZero
      Just (TmSucc nv1) -> Just nv1
      _                 -> Nothing

-- B-IsZeroZero and B-IsZeroSucc
big (TmIsZero t1)
  = case big t1 of
      Just TmZero       -> Just TmTrue
      Just (TmSucc nv1) -> Just TmFalse
      _                 -> Nothing

-- B-Value
big t | isVal t   = Just t
      | otherwise = Nothing
