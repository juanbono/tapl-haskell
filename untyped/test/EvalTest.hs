module EvalTest where

import Language.Untyped.Syntax
import Language.Untyped.Eval
import Language.Untyped.Context
import           Terms
import Test.Tasty.Discover (hspec, describe, it, shouldBe)
