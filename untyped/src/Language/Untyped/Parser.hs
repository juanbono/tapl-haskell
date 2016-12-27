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
  = chainl1 parseNonApp $ do
  info <- infoFrom <$> getPosition
  return $ TmApp info

parseNonApp :: Parser Term
parseNonApp
  =  parseVar
 <|> parseAbs
 <|> parens parseTerm

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
  dot
  ctx <- getState
  setState $ addName ctx v
  term <- parseTerm
  setState ctx
  info <- infoFrom <$> getPosition
  return $ TmAbs info v term

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parseString :: String -> Either ParseError Term
parseString = runParser parseTerm emptyContext ""

-- tests
v1, v2, v3, v4 :: String

v1 = "\\x.x"

v2 = "(\\x.x) x"

v3 = "(\\x . (\\y. y) x)"

v4 = "(\\x. (\\y.y) t)"
