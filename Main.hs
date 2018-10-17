{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser "1-2-3"
  runParser "(((9)))"
  runParser "1*2-3/4+5"
  runParser "!"
  runParser "  1   +  ( (  2 +   3  ) )  "
  runParser "123^2^3"
  runParser "[1,2,3]"
  runParser "[1, 2, [2 + 3, val = 123^15 + 10, [], [zero, one, two, 3, 4, 5 + 0, val_1 = 6]], lol]; same; 777"
  runParser " f "
  runParser "f   ;  f; f "
  runParser "f   ;  f; f; but wrong; "
  runParser "test ; x = 13; y = z = 42 + 6; 777"
  runParser "[[[[[]]]]]"
  runParser " -(3-4)"
  runParser "[1] ++ [a]"
  runParser "a ++ a"
  runParser "[1] ++ a"
  runParser "a ++ [1]"
  runParser "a ++ 1"
  runParser "[1 + 2, 3 + 4, [7, 8, 9]] ++ [a]; x = []; [1, 2, 3]"
  runParser "a = [1] ++ b ++ [[2] ++ [c, d, [[e]] ++ [[d]]]]"
  runParser "-1"
  runParser "-1-2"
  runParser "-1--2"
  runParser "-(-(-1))-(-(-2))"