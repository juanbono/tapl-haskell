module Language.Arith.Lexer
  (
    reserved
  , parens
  , whiteSpace
  ) where

import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef { Token.reservedNames = [ "if"
                                   , "then"
                                   , "else"
                                   , "true"
                                   , "false"
                                   , "0"
                                   , "succ"
                                   , "pred"
                                   , "iszero"
                                   ]
           }

lexer = Token.makeTokenParser languageDef

reserved = Token.reserved lexer

parens = Token.parens lexer

whiteSpace = Token.whiteSpace lexer
