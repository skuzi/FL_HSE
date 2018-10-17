module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem, take)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AAssign String AST
         | ANum Char String
         | AIdent Char String
         | AUnOp Char AST
         | AList AST
         | ASemCol AST AST
         | AComma AST AST
         | AConcat AST AST
         | AEmpty


{-case expression input of
           Success (tree, ts') ->
             if null ts'
             then Just (Success tree)
             else Just (Error ("Syntax error on: " ++ show ts')) -- Only a prefix of the input is parsed
           Error err -> Just (Error err) -- Legitimate syntax error-}


-- TODO: Rewrite this without using Success and Error
parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _  -> Just (take input)

take :: String -> (Result AST)
take input = 
  case statement input of 
    Success(tree, ts') ->
      if null ts'
      then 
        Success tree
      else
        Error ("Syntax error on:" ++ show ts')
    Error err -> Error err

statement :: Parser AST
statement = 
  (
    base >>= \b -> 
    semicol |>
    statement >>= \st -> return (ASemCol b st)
  )
  <|> base

base :: Parser AST
base = list <|> expression

listexpr :: Parser AST
listexpr = 
  ( identifier >>= \(AIdent s i) ->
    assignment |>
    listexpr >>= \le -> return (AAssign i le)
  )
  <|>  ( baselist     >>= \ll ->
         conc         |>
         listexpr     >>= \lr -> return (AConcat ll lr)
       )
  <|> baselist

baselist :: Parser AST
baselist = 
  ( lbrac |>
    elems >>= \el ->
    rbrac |> return (AList el)
  )
  <|> (lbrac |>
       rbrac |> return (AList (AEmpty))
      )
  <|> identifier

elems :: Parser AST
elems = 
  ( expression >>= \e ->
    comma  |>
    elems >>= \el -> return (AComma e el)
  )
  <|> ( listexpr >>= \e ->
        comma  |>
        elems >>= \el -> return (AComma e el)
      )
  <|> list
  <|> expression

list :: Parser AST
list =
  ( baselist >>= \ll ->
        conc     |>
        listexpr >>= \lr -> return (AConcat ll lr) 
      )
  <|> ( lbrac |>
    elems >>= \el ->
    rbrac |> return (AList el)
  )
  <|> ( lbrac |>
        rbrac |> return (AList (AEmpty))
  )
  <|> ( identifier >>= \(AIdent '\0' i) ->
        assignment |>
        list >>= \l -> return (AAssign i l)
    )


expression :: Parser AST
expression =
  ( identifier >>= \(AIdent '\0' i) ->
    assignment |>
    expression >>= \e -> return (AAssign i e)
  )
  <|> ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>= \op ->
        expression >>= \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  factor >>= \l ->
  ( ( divMult >>= \op ->
      term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

factor :: Parser AST
factor =
  ( lparen |>
    expression >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> ( minus >>= \s ->
        lparen |>
        expression >>= \e ->
        rparen |> return (AUnOp s e)
      )
  <|> identifier
  <|> ( minus >>= \s ->
        identifier >>= \i -> return (AUnOp s i)
      )
  <|> digit
  <|> (
      minus >>= \s ->
      digit >>= \d -> return (AUnOp s d)
    )

digit :: Parser AST
digit      = map (ANum '\0'  .  T.pnum) (sat T.isNumber num)

identifier :: Parser AST
identifier = map (AIdent '\0' .  T.word) (sat T.isWord ident)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

lbrac :: Parser Char
lbrac = char '['

rbrac :: Parser Char
rbrac = char ']'

comma :: Parser Char
comma = char ','

semicol :: Parser Char
semicol = char ';'

minus :: Parser Char
minus = char '-'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*' <|> char '^')

conc :: Parser String
conc = (sat (== "++") elemm)




instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum  s i    -> s : i
                  AIdent s i   -> s : i
                  AList l      -> "[\n" ++ show' (ident n) l ++ "]"
                  ASemCol f s  -> show' 0 f ++ "\n;\n" ++ show' 0 s
                  AComma f s   -> ",\n" ++ show' n f ++ "\n" ++ show' n s
                  AConcat l r  -> "++\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AUnOp s i    -> s : show' 0 i
                  AEmpty       -> ""


                  )
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Pow   = '^'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'