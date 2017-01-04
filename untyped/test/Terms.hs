module Terms
  ( parsedIdentity
  , parsedYCombinator
  , parsedSomeAbstraction
  ) where
import Language.Untyped.Syntax

parsedIdentity :: Term
parsedIdentity -- \x.x
  = TmAbs "x" (TmVar 0)

parsedYCombinator :: Term
parsedYCombinator -- \f. (\x. f (x x)) (\x. f (x x))
  = TmAbs "f" $
      TmApp (TmAbs "x" (TmApp (TmVar 1) (TmApp (TmVar 0) (TmVar 0))))
            (TmAbs "x" (TmApp (TmVar 1) (TmApp (TmVar 0) (TmVar 0))))

parsedSomeAbstraction :: Term
parsedSomeAbstraction -- \x. (\y.y) x
  = TmAbs "x" (TmApp (TmAbs "y" (TmVar 0)) (TmVar 0))
