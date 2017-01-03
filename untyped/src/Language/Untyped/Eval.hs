module Language.Untyped.Eval
  (
    small
  , multi
  , big
  ) where

import Language.Untyped.Syntax
import Language.Untyped.Context

isVal :: Context -> Term -> Bool
isVal _ (TmAbs _ _) = True
isVal _ _ = False

small :: Context -> Term -> Maybe Term
small ctx (TmApp (TmAbs x t12) v2)
  | isVal ctx v2 = Just $ termSubstTop v2 t12
small ctx (TmApp v1 t2)
  | isVal ctx v1 = case small ctx t2 of
                     Just t2' -> Just $ TmApp v1 t2'
                     Nothing -> Nothing
small ctx (TmApp t1 t2)
  = case small ctx t1 of
      Just t1' -> Just $ TmApp t1' t2
      Nothing -> Nothing
small _ _ = Nothing

multi :: Context -> Term -> Maybe Term
multi ctx t = do
  t' <- small ctx t
  multi ctx t'

big :: Context -> Term -> Maybe Term
big = undefined

-- tests
v1, v2, v3, v4 :: Term
v1 = undefined

v2 = undefined

v3 = undefined

v4 = undefined
