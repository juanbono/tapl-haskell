module Language.Untyped.PrettyPrinting
  (
    showTerm
  , toPP
  ) where

import Printcess.PrettyPrinting
import Language.Untyped.Syntax
import Language.Untyped.Context
import Data.Monoid
import Control.Annihilator

data PPTerm
  = PPVar String
  | PPAbs String PPTerm
  | PPApp PPTerm PPTerm
  | PPBadTerm CtxException

toPP :: Context -> Term -> PPTerm
toPP ctx (TmVar info index n)
  = case fromIndex info ctx index of
                        Left e     -> PPBadTerm e
                        Right name -> PPVar name
toPP ctx (TmAbs _ name term)
  = let (ctx', name') = pickFreshName ctx name
    in PPAbs name' (toPP ctx' term)
toPP ctx (TmApp _ t1 t2)
  = PPApp (toPP ctx t1) (toPP ctx t2)

instance Pretty PPTerm where
  pp (PPVar name) = pp name
  pp (PPAbs name term) = assocR 0 $ "λ" +> I name +> "." ~> R term
  pp (PPApp t1 t2) = assocL 9 $ L t1 ~> R t2
  pp (PPBadTerm (Unbound name)) = pp $ "Unbound: " ++ name
  pp (PPBadTerm (NotFound info)) = pp $ "Identifier not found at: " ++ show info

showTerm :: Context -> Term -> PrettyM ()
showTerm ctx = pp . toPP ctx
