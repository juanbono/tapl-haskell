module Language.Untyped.Lexer
  (
    reserved
  , parens
  , dot
  , identifier
  ) where

import           Data.Functor.Identity
import           Text.Parsec
import qualified Text.Parsec.Token     as T

languageDef
  = T.LanguageDef { T.commentStart = ""
                  , T.commentEnd   = ""
                  , T.commentLine  = ""
                  , T.nestedComments = False
                  , T.identStart = letter
                  , T.identLetter = alphaNum
                  , T.opStart = letter
                  , T.opLetter = alphaNum
                  , T.reservedOpNames = ["\\", "λ"]
                  , T.reservedNames = []
                  , T.caseSensitive = True
                }

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser languageDef

--dot :: ParsecT String u Identity String
dot = T.dot lexer

--identifier :: ParsecT String u Identity String
identifier = T.identifier lexer

--parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = T.parens lexer

--reserved :: String -> ParsecT String u Identity ()
reserved = T.reserved lexer
