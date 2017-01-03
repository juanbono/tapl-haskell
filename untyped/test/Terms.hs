module Terms
  (
    parsedIdentity
  , parsedYCombinator
  , parsedSomeAbstraction
  ) where
import Language.Untyped.Syntax

parsedIdentity :: Term
parsedIdentity -- \x.x
  = TmAbs "x" (TmVar 0 1)

parsedYCombinator :: Term
parsedYCombinator -- \f. (\x. f (x x)) (\x. f (x x))
  = TmAbs "f" $
      TmApp (TmAbs "x" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 0 2))))
            (TmAbs "x" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 0 2))))

parsedSomeAbstraction :: Term
parsedSomeAbstraction -- \x. (\y.y) x
  = TmAbs "x" (TmApp (TmAbs "y" (TmVar 0 2))
                     (TmVar 0 1))

-- Church Booleans
churchFalse :: Term
churchFalse = undefined

churchTrue :: Term
churchTrue = undefined

churchAnd :: Term
churchAnd = undefined

churchOr :: Term
churchOr = undefined

churchNot :: Term
churchNot = undefined

-- Church Numerals
churchZero :: Term
churchZero = undefined

churchOne :: Term
churchOne = undefined

churchTwo :: Term
churchTwo = undefined

churchThree :: Term
churchThree = undefined

-- Operations on Church Numerals
