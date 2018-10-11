module Main where

import Parser
import Tokenizer

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ tokenize input
  print $ parse input
  putStrLn ""

main :: IO ()
main = do
  runParser " v2ar_1 = 1 - 2 - 3 "
 -- run
  runParser " var = (1-2-3)"
  runParser " var = -1-2-3"
  runParser " (2^(2+3))^4"
  runParser " 1 * 2 - 3 / 4 + 5"
  runParser " maxval = 13 + 12 + val "