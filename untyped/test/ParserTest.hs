module ParserTest
  (
    case_spec
  ) where

import           Language.Untyped.Parser
import           Language.Untyped.Syntax
import           Terms
import Test.Tasty.Discover (hspec, describe, it, shouldBe)

stringyIdentity :: String
stringyIdentity = "\\x.x"

stringyYCombinator :: String
stringyYCombinator = "\\f. (\\x. f (x x)) (\\x. f (x x))"

someAbstraction :: String
someAbstraction = "(\\x . (\\y. y) x)"


case_spec
  = hspec $ describe "The parser" $ do
      it "parses correctly an abstraction with an application inside"
        $ parseString someAbstraction `shouldBe` Right parsedSomeAbstraction

      it "parse the identity combinator"
        $ parseString stringyIdentity `shouldBe` Right parsedIdentity

      it "parse the Y combinator"
        $ parseString stringyYCombinator `shouldBe` Right parsedYCombinator
