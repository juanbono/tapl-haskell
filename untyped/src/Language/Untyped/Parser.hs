module Language.Untyped.Parser
  (
    parseString
  , parseTerm
  ) where

import           Control.Monad
import           Data.Functor.Identity
import           Language.Untyped.Context
import           Language.Untyped.Lexer
import           Language.Untyped.Syntax
import           Text.Parsec

-- | Parser type who uses String as Stream Type and Context as State.
type Parser = Parsec String Context

parseTerm :: Parser Term
parseTerm
  = chainl1 parseNonApp $ return TmApp

parseNonApp :: Parser Term
parseNonApp
  =  parseVar
 <|> parseAbs
 <|> parens parseTerm

parseVar :: Parser Term
parseVar = do
  var <- identifier
  ctx <- getState
 -- setState $ addName ctx var
  newCtx <- getState
  let idx = toIndex newCtx var
  case idx of
    Left e  -> error $ show e
    Right i -> return $ TmVar i (length newCtx)

parseAbs :: Parser Term
parseAbs = do
  char '\\' <|> char 'λ'
  v <- identifier
  dot
  ctx <- getState
  setState $ addName ctx v
  term <- parseTerm
  setState ctx
  return $ TmAbs v term

parseString :: String -> Either ParseError Term
parseString = runParser parseTerm emptyContext ""

-- tests
v2, v3, v4 :: String
v2 = "(\\x.x) x" -- da bien creo, hacer cuentas

v3 = "(\\x . (\\y. y) x)" -- da bien

v4 = "(\\x. (\\y.y) t)" -- tira error, pero esta bien :D
