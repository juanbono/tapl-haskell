module Language.Untyped.PrettyPrinting
  ( showTerm
  , toPP
  ) where

import           Language.Untyped.Context
import           Language.Untyped.Syntax
import           Printcess.PrettyPrinting

toPP :: Context -> NamelessTerm -> Term
toPP ctx (NmVar index)
  = case fromIndex ctx index of
      Left e     -> error (show e) --BadTerm e
      Right name -> Var name
toPP ctx (NmAbs name term)
  = let (ctx', name') = pickFreshName ctx name
    in Abs name' (toPP ctx' term)
toPP ctx (NmApp t1 t2)
  = App (toPP ctx t1) (toPP ctx t2)

instance Pretty Term where
  pp (Var name)      = pp name
  pp (Abs name term) = assocR 0 $ "λ" +> I name +> "." ~> R term
  pp (App t1 t2)     = assocL 9 $ L t1 ~> R t2
--  pp (BadTerm (Unbound name)) = pp $ "Unbound: " ++ name
--  pp PBadTerm NotFound)       = pp "Identifier not found at: "

showTerm :: Context -> NamelessTerm -> PrettyM ()
showTerm ctx = pp . toPP ctx

