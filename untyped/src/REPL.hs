module REPL
  (
    repl
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.List                (isPrefixOf)
import           Data.List                (isPrefixOf)
import           Language.Untyped.Context
import           Language.Untyped.Eval
import           Language.Untyped.Parser  (parseString, parseTerm)
import           Language.Untyped.Syntax
import           Rainbow
import           System.Console.Repline
import           Text.Parsec
import           Printcess.PrettyPrinting
import           Language.Untyped.PrettyPrinting

type Repl a = HaskelineT IO a

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

cmd :: String -> Repl ()
cmd = cmdWith id id

completer :: Monad m => WordCompleter m
completer n = do
  let keywords
        = []
  return $ filter (isPrefixOf n) keywords

help :: [String] -> Repl ()
help _ = liftIO $ putChunkLn . chunk  $ "Help: " ++ show helpText
  where
    helpText = "Some help"

evalWith :: (Term -> Maybe Term) -> [String] -> Repl ()
evalWith f = cmdWith (printTerm . f) (fore green) . unwords

printTerm :: Maybe Term -> String
printTerm (Just t)  = pretty defConfig $ showTerm [] t -- arreglar
printTerm (Nothing) = "*** Stuck ***"

options :: [(String, [String] -> Repl ())]
options = [
  ("help", help),
  ("q", const abort)
  ]

ini :: Repl ()
ini = liftIO $ putChunkLn . fore yellow . chunk $ txt
  where
    txt = "λ: Untyped Lambda Calculus"

repl :: IO ()
repl = evalRepl "λ> " cmd options (Word completer) ini

main :: IO ()
main = repl
