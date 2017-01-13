module REPL
  (
    repl
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.List                (isPrefixOf)
import           Language.Untyped.Context
import           Language.Untyped.Eval
import           Language.Untyped.Parser2
import           Language.Untyped.Syntax
import           Rainbow
import           System.Console.Repline
import           Text.Parsec
import           Printcess.PrettyPrinting
import           Language.Untyped.PrettyPrinting

type Repl a = HaskelineT IO a
{-
cmdWith
  :: Show b
  => (Term -> b)
  -> (Chunk String -> Chunk String)
  -> String
  -> Repl ()
cmdWith f g input =
  do
    let ctx = emptyContext
        parsedString = runParser parseTerm ctx "" input
        msg = case parsedString of
                 Left err -> chunk (show err) & fore red
                 Right t  -> (g . chunk . pretty defConfig . showTerm ctx) t
    liftIO $ putChunkLn msg
-}
cmdWith
  :: Show b
  => (ParseResult -> b)
  -> (Chunk String -> Chunk String)
  -> String
  -> Repl ()
cmdWith f g input = do
    let parsedString = runParser parseTerm emptyContext "" input
        msg = case parsedString of
                 Left err -> chunk (show err) & fore red
                 Right (PR t c) -> (g . chunk . pretty defConfig . showTerm c) t
    liftIO $ putChunkLn msg

cmd :: String -> Repl ()
cmd = cmdWith id id

completer :: Monad m => WordCompleter m
completer n = do
  let keywords = []
  return $ filter (isPrefixOf n) keywords

help :: [String] -> Repl ()
help _ = liftIO $ putChunkLn . chunk  $ "Help: " ++ show helpText
  where
    helpText = "Some help"

evalWith :: (ParseResult -> ParseResult) -> [String] -> Repl ()
evalWith f = cmdWith (printTerm . f) (fore green) . unwords

printTerm :: ParseResult -> String
printTerm (PR term ctx) = pretty defConfig $ showTerm [] term

smallEval :: ParseResult -> NamelessTerm
smallEval (PR t ctx)
  = let t' = small ctx t
    in t'

options :: [(String, [String] -> Repl ())]
options
  = [ ("help" , help)
    , ("q"    , const abort)
    , ("small", evalWith $ \(PR t c) -> PR (small c t) c)
    , ("multi", evalWith $ \(PR t c) -> PR (multi c t) c)
    , ("big"  , error "not implemented")
    ]

ini :: Repl ()
ini = liftIO $ putChunkLn . fore yellow . chunk $ txt
  where
    txt = "λ: Untyped Lambda Calculus"

repl :: IO ()
repl = evalRepl "λ> " cmd options (Word completer) ini

main :: IO ()
main = repl
