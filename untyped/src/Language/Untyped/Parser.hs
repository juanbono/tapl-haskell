module Language.Untyped.Parser
  (
    parseString
  ) where

import Text.Parsec
import Language.Untyped.Context
import Language.Untyped.Syntax
import Language.Untyped.Lexer
import Control.Monad
import Data.Functor.Identity

type Parser a = ParsecT String Context Identity a

parseTerm :: Parser Term
parseTerm
  = chainl1 parsers $ do
  space
  pos <- getPosition
  return $ TmApp (infoFrom pos)
    where parsers = (parseAbs <|> parseVar <|> parens parseTerm)

parseVar :: Parser Term
parseVar = do
  var <- identifier
  ctx <- getState
  info <- infoFrom <$> getPosition
  let idx = toIndex ctx var
  return $ TmVar info idx (length ctx)

parseAbs :: Parser Term
parseAbs = do
  char '\\' <|> char 'λ'
  v <- identifier
  modifyState (\c -> addName c v)
  dot
  term <- parseTerm
  modifyState tail
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v term

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parseString :: String -> Either ParseError Term
parseString = runParser parseTerm emptyContext ""
