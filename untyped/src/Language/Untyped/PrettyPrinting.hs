module Language.Untyped.PrettyPrinting
  (
    showTerm
  , toPP
  ) where

import           Language.Untyped.Context
import           Language.Untyped.Syntax
import           Printcess.PrettyPrinting

toPP :: Context -> Term -> PPTerm
toPP ctx (TmVar index)
  = case fromIndex ctx index of
      Left e     -> PPBadTerm e
      Right name -> PPVar name
toPP ctx (TmAbs name term)
  = let (ctx', name') = pickFreshName ctx name
    in PPAbs name' (toPP ctx' term)
toPP ctx (TmApp t1 t2)
  = PPApp (toPP ctx t1) (toPP ctx t2)

instance Pretty PPTerm where
  pp (PPVar name)               = pp name
  pp (PPAbs name term)          = assocR 0 $ "λ" +> I name +> "." ~> R term
  pp (PPApp t1 t2)              = assocL 9 $ L t1 ~> R t2
  pp (PPBadTerm (Unbound name)) = pp $ "Unbound: " ++ name
  pp (PPBadTerm NotFound)       = pp "Identifier not found at: "

showTerm :: Context -> Term -> PrettyM ()
showTerm ctx = pp . toPP ctx
