module Tokenizer where

data Token = TNum Integer
           | TIdent [Char]
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Pow
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize (c : cs) | isOperator c   = TOp (operator c) : tokenize cs
                  | isDigit c      = TNum (num $ reverse cNum) : tokenize csNum
                  | isAlpha c      = TIdent (alpha cId) : tokenize csId
                  | c == '('       = TLParen : tokenize cs
                  | c == ')'       = TRParen : tokenize cs
                  | c == '='       = TAssign : tokenize cs
                  | isWhiteSpace c = tokenize cs
                  | otherwise = error ("Lexical error: unacceptable character " ++ [c])
                  where (cId, csId) = getId (c : cs)
                        (cNum, csNum) = getNum (c : cs)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Pow
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

digit :: Char -> Integer
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")


getNum :: [Char] -> ([Char], [Char])
getNum ([]) = ([], [])
getNum ((c : cs)) | isDigit c = (c : x, y)
                  where (x, y) = getNum cs
getNum c = ([], c)

num :: [Char] -> Integer
num [] = 0
num (c : cs) = digit c + (num cs) * 10

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z'] ++ ['_'] ++ ['1' .. '9']

alpha :: [Char] -> [Char]
alpha c = c

getId :: [Char] -> ([Char], [Char])
getId [] = ([], [])
getId (c : cs) | isAlpha c = (c : x, y)
               where (x, y) = getId cs
getId c = ([], c)

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"