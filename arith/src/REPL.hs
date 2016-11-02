module REPL
  (
    repl
  ) where

import Language.Arith.Syntax
import Language.Arith.Parser (parseString)
import System.Console.Repline
import Rainbow
import Data.List (isPrefixOf)
import Data.List (isPrefixOf)
import Control.Applicative
import Control.Monad.IO.Class

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd = liftIO . print . parseString

completer :: Monad m => WordCompleter m
completer n = do
  let keywords
        = ["true","succ","pred","0","false","if <cond> then <true> else <false>"]
  return $ filter (isPrefixOf n) keywords

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

options :: [(String, [String] -> Repl ())]
options = [
  ("help", help)
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Arith: arithmetical and boolean expressions"

repl :: IO ()
repl = evalRepl "Arith> " cmd options (Word completer) ini
