module Tokenizer where

data Token = TNum [Char]
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
                  | isDigit c      = TNum (cNum) : tokenize csNum
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

digit :: Char -> Char
digit c = c
digit c = error ("Lexical error: " ++ c : " is not a digit!")


getNum :: [Char] -> ([Char], [Char])
getNum ([]) = ([], [])
getNum ((c : cs)) | isDigit c = (c : x, y)
                  where (x, y) = getNum cs
getNum c = ([], c)

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