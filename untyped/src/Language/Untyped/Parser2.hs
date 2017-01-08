module Language.Untyped.Parser2
  ( parseTerm
  , parseString
  , ParseResult (..)
  ) where

import           Control.Monad
import           Data.Functor.Identity
import           Language.Untyped.Context
import           Language.Untyped.Lexer
import           Language.Untyped.Syntax
import           Text.Parsec
import           Data.List

-- | Parser type who uses String as Stream Type and Context as State.
type Parser = ParsecT String Context Identity

data ParseResult
  = PR { term :: Term
       , ctx  :: Context
       } deriving (Show, Eq)

parseTerm :: Parser ParseResult
parseTerm = do
  pr <- chainl1 parseNonApp $ return (\t1 t2 -> PR (TmApp (term t1) (term t2)) (ctx t1 `union` ctx t2))
  let t = term pr
  ctx  <- getState
  return $ PR t ctx

parseNonApp :: Parser ParseResult
parseNonApp
  =  parseVar
 <|> parseAbs
 <|> parens parseTerm

parseVar :: Parser ParseResult
parseVar = do
  var <- identifier
  ctx <- getState
  modifyState (`addName` var)
  newCtx <- getState
  let idx = toIndex ctx var
  case idx of
    Left e  -> error $ show e
    Right i -> return $ PR (TmVar i) newCtx

parseAbs :: Parser ParseResult
parseAbs = do
  char '\\' <|> char 'λ'
  v <- identifier
  dot
  modifyState (`addName` v)
  pr <- parseTerm
  let t = term pr
  ctx <- getState
  return $ PR (TmAbs v t) ctx

parseString :: String -> Either ParseError ParseResult
parseString = runParser parseTerm emptyContext ""

v3 = "(\\x . (\\y. y) x)" -- da bien
