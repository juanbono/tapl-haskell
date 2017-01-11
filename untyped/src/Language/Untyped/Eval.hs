module Language.Untyped.Eval
  ( small
  , multi
  , big
  ) where

import qualified Language.Untyped.Church         as Church
import           Language.Untyped.Context
import           Language.Untyped.PrettyPrinting
import           Language.Untyped.Syntax
import           Printcess.PrettyPrinting

isVal :: Context -> NamelessTerm -> Bool
isVal _ (NmAbs _ _) = True
isVal _ _           = False

small :: Context -> NamelessTerm -> NamelessTerm
small ctx (NmApp t1 t2)
  -- E-AppAbs
  | isVal ctx t2 = let (NmAbs t11 t12) = t1
                   in termSubstTop t2 t12
  -- E-App2
  | isVal ctx t1 = let t2' = small ctx t2
                   in NmApp t1 t2'
  -- E-App1
  | otherwise    = let t1' = small ctx t1
                   in NmApp t1' t2
small _ t = t

multi :: Context -> NamelessTerm -> NamelessTerm
multi ctx t = let t' = small ctx t in small ctx t'

big :: Context -> NamelessTerm -> NamelessTerm
big = undefined

-- funciones bugueadas pero utiles para testear (?)
bindings :: NamelessTerm -> Context
bindings (NmVar _)     = []
bindings (NmAbs b t1)  = (b, NameBind) : bindings t1
bindings (NmApp t1 t2) = bindings t1 ++ bindings t2

eval1 :: NamelessTerm -> NamelessTerm
eval1 t = small (bindings t) t

eval2 :: NamelessTerm -> NamelessTerm
eval2 t = multi (bindings t) t

evalAndPrint1 :: NamelessTerm -> String
evalAndPrint1 t = let term = eval1 t
                  in (pretty defConfig . showTerm emptyContext) term

evalAndPrint2 :: NamelessTerm -> String
evalAndPrint2 t = let term = eval2 t
                  in (pretty defConfig . showTerm (bindings term)) term
