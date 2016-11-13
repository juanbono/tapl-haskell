module REPL
  (
    repl
  ) where

import Language.Arith.Syntax
import Language.Arith.Eval
import Language.Arith.Parser (parseString)
import System.Console.Repline
import Rainbow
import Data.List (isPrefixOf)
import Data.List (isPrefixOf)
import Control.Applicative
import Control.Monad.IO.Class

type Repl a = HaskelineT IO a

cmdWith ::
  Show b => (Term -> b) -> (Chunk String -> Chunk String) -> String -> Repl ()
cmdWith f g input =
  let msg = case parseString input of
              Left err -> chunk (show err) & fore red
              Right t  -> (g . chunk . show . f) t
  in
    liftIO $ putChunkLn msg

cmd :: String -> Repl ()
cmd = cmdWith id id

completer :: Monad m => WordCompleter m
completer n = do
  let keywords
        = ["true","succ","pred","0","false","if <cond> then <true> else <false>"]
  return $ filter (isPrefixOf n) keywords

help :: [String] -> Repl ()
help args = liftIO $ putChunkLn . chunk  $ "Help: " ++ show args

evalWith :: (Term -> Maybe Term) -> [String] -> Repl ()
evalWith f = cmdWith (printTerm . f) (fore green) . unwords

printTerm :: Maybe Term -> String
printTerm (Just t) = show t
printTerm (Nothing) = "*** Stuck ***"

options :: [(String, [String] -> Repl ())]
options = [
  ("help", help),
  ("q", const abort),
  ("small", evalWith small),
  ("multi", evalWith multi),
  ("big", evalWith big)
  ]

ini :: Repl ()
ini = liftIO $ putChunkLn . fore yellow . chunk $ txt
  where
    txt = "Arith: arithmetical and boolean expressions"

repl :: IO ()
repl = evalRepl "Arith> " cmd options (Word completer) ini

main = repl
