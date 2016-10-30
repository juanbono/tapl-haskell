module Parser
  (
  ) where

import Syntax
import Text.ParserCombinators.Parsec

parseValue :: Parser Term
parseValue
