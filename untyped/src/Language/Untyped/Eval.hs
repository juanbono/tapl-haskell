module Language.Untyped.Eval
  (
    small
  , multi
  , big
  ) where

import qualified Language.Untyped.Church         as Church
import           Language.Untyped.Context
import           Language.Untyped.PrettyPrinting
import           Language.Untyped.Syntax
import           Printcess.PrettyPrinting

isVal :: Context -> Term -> Bool
isVal _ (TmAbs _ _) = True
isVal _ _           = False

small :: Context -> Term -> Term
small ctx (TmApp t1 t2)
  -- E-AppAbs
  | isVal ctx t2 = let (TmAbs t11 t12) = t1
                   in termSubstTop t2 t12
  -- E-App2
  | isVal ctx t1 = let t2' = small ctx t2
                   in TmApp t1 t2'
  -- E-App1
  | otherwise    = let t1' = small ctx t1
                   in TmApp t1' t2
small _ t = t

multi :: Context -> Term -> Term
multi ctx t = let t' = small ctx t in small ctx t'

e = TmApp (TmApp (TmAbs "t" (TmAbs "f" (TmVar 0))) (TmAbs "t" (TmAbs "f" (TmVar 0)))) (TmAbs "t" (TmAbs "f" (TmVar 1)))

big :: Context -> Term -> Term
big = undefined

-- funciones bugueadas pero utiles para testear (?)
bindings :: Term -> Context
bindings (TmVar _)     = []
bindings (TmAbs b t1)  = (b, NameBind) : bindings t1
bindings (TmApp t1 t2) = bindings t1 ++ bindings t2

eval1 :: Term -> Term
eval1 t = small (bindings t) t

eval2 :: Term -> Term
eval2 t = multi (bindings t) t

evalAndPrint1 :: Term -> String
evalAndPrint1 t = let term = eval1 t
                 in (pretty defConfig . showTerm emptyContext) term

evalAndPrint2 :: Term -> String
evalAndPrint2 t = let term = eval2 t
                 in (pretty defConfig . showTerm (bindings term)) term
