module Parser where

import Control.Applicative
import Data.Char


data AST =  Num Integer       |
            BinOp AST [AST]   |
            Symbol Char       |
            Keyword String    |
            Variable String   |
            Lambda [AST] AST [AST] |
            Define AST AST    |
            If AST AST AST    |
            Error String      
            deriving (Show)

newtype Parser a = Parser {
  runParser :: String -> Either String (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser run
    where
      run str = do
        (a, str') <- p str
        return (f a, str')

instance Applicative Parser where
  pure a = Parser run
    where
      run str = return (a, str)
  Parser p <*> Parser q = Parser run
    where
      run str = do
        (f, str') <- p str
        (a, str'') <- q str'
        return (f a, str'')

instance Monad Parser where
  Parser p >>= f = Parser run
    where
      run str = do
        (a, str') <- p str
        let (Parser b) = f a
        b str'

instance Alternative Parser where
  empty = Parser run
    where
      run str = Left "error"
  Parser p <|> Parser q = Parser run
    where
      run str = case p str of
        Left _ -> q str
        Right b -> Right b

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser run
  where
    run [] = Left "error empty st"
    run (x:xs) = if f x then Right (x, xs) else Left "error satisfy"

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = mapM char

possible_char :: Parser Char
possible_char = satisfy (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<>?/.,;:'\"[]{}\\|`~!@#$%^&*_+-=")

possible_string :: Parser String
possible_string = do
  c <- possible_char
  cs <- many possible_char
  return (c:cs)

digit :: Parser Char
digit = satisfy (`elem` "0123456789")

digitOrSign :: Parser Char
digitOrSign = satisfy (`elem` "-+0123456789")

digits :: Parser String
digits = do
  c <- digitOrSign
  cs <- many digit
  return (c:cs)

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

variable :: Parser String
variable = do
  c <- letter
  cs <- many alphanum
  return (c:cs)

space :: Parser Char
space = char ' ' <|> char '\n' <|> char '\t' <|> char '\r'

spaces :: Parser String
spaces = many space

number :: Parser AST
number = do
  spaces
  n <- digits
  if n !! 0 == '+'
    then return (Num (read (tail n)))
    else
      if n !! 0 == '-' && length n <= 1
        then return (Error "error number")
        else return (Num (read n))

condition_keyword :: Parser AST
condition_keyword = do
  str <- string "#f" <|> string "#t"
  return (Keyword str)

possible_keyword :: Parser AST
possible_keyword = do
  str <- possible_string
  return (Keyword str)

expr :: Parser AST
expr = do
  spaces
  ast <- isif <|> condition_keyword <|> isdefine <|> islambda <|> binop <|> isvariable <|> number <|> parentesis
  return ast

isif :: Parser AST
isif = do
  string "if"
  spaces
  t1 <- number <|> parentesis <|> condition_keyword <|> isvariable
  spaces
  t2 <- parentesis <|> number <|> condition_keyword <|> isvariable
  spaces
  t3 <- parentesis <|> number <|> condition_keyword <|> isvariable
  spaces
  return (If t1 t2 t3)

isvariable :: Parser AST
isvariable = do
  spaces
  t1 <- possible_string
  spaces
  return (Variable t1)

isdefine :: Parser AST
isdefine = do
  string "define"
  t1 <- isvariable <|> parentesis
  t2 <- number <|> isvariable <|> islambda <|> parentesis
  spaces
  return (Define t1 t2)

-- (define (add a b) 5)
islambdaParam :: Parser AST
islambdaParam = do
  spaces
  isvariable

list :: Parser [AST]
list = do
  char '('
  t <- many islambdaParam
  spaces
  char ')'
  return t

islambda :: Parser AST
islambda = do
  char '('
  spaces
  string "lambda"
  spaces
  t1 <- list
  spaces
  t2 <- parentesis
  spaces
  char ')'
  ts <- many (parentesis <|> number <|> isvariable)
  spaces
  return (Lambda t1 t2 ts)

binop :: Parser AST
binop = do
  op <- possible_keyword
  spaces
  ts <- many (parentesis <|> number <|> isvariable)
  spaces
  return (BinOp op ts)

program:: Parser [AST]
program = many (parentesis <|> number <|>  isvariable)

parentesis :: Parser AST
parentesis = do
  spaces
  char '('
  e <- expr
  char ')'
  spaces
  return e
