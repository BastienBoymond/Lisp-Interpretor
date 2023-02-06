module Evaluate (
    evaluate
) where

import Parser
import Data.Tuple
import Data.List


intToNum :: [Integer] -> [AST]
intToNum = map Num

evaluate_function :: (AST, AST) -> [Integer] -> [(AST, AST)] -> Either String (String, Integer)
evaluate_function ((BinOp (Keyword s) xs), func) a vars = 
    if length a == length xs then
         evaluate [func] (assigned_variables xs a vars)
    else 
        Left "error number of arguments"
evaluate_function ((Variable name), (Lambda params body values)) a vars =
    evaluate [(Lambda params body (intToNum a))] vars


assigned_variables :: [AST] -> [Integer] -> [(AST, AST)] -> [(AST, AST)]
assigned_variables v n vars = zip v (intToNum n) ++ vars

evaluate_keyword_binop :: String -> Integer -> Integer -> Integer
evaluate_keyword_binop "+" a b = (a + b)
evaluate_keyword_binop "-" a b = (a - b)
evaluate_keyword_binop "*" a b = (a * b)
evaluate_keyword_binop "<" a b = (if a < b then 1 else 0)
evaluate_keyword_binop ">" a b = (if a > b then 1 else 0)
evaluate_keyword_binop "div" a b = (a `div` b)
evaluate_keyword_binop "mod" a b= (a `mod` b)
evaluate_keyword_binop "eq?" a b  | a == b = 1
                            | otherwise = 0
evaluate_keyword_binop "<=" a b  | a <= b = 1
                            | otherwise = 0
evaluate_keyword_binop ">=" a b  | a >= b = 1
                            | otherwise = 0
evaluate_keyword_binop _ _ _ = 0

evaluate_builtin :: String -> [Integer] -> [(AST, AST)] -> Integer ->  Either String (String, Integer)
evaluate_builtin "+" (a:b:as) vars _ = (evaluate_builtin "+" ((evaluate_keyword_binop "+" a b):as) vars 1)
evaluate_builtin "-" (a:b:as) vars _ = (evaluate_builtin "-" ((evaluate_keyword_binop "-" a b):as) vars 1)
evaluate_builtin "*" (a:b:as) vars _ = (evaluate_builtin "*" ((evaluate_keyword_binop "*" a b):as) vars 1)
evaluate_builtin "<" (a:b:as) vars _ = (evaluate_builtin "<" ((evaluate_keyword_binop "<" a b):as) vars 1)
evaluate_builtin ">" (a:b:as) vars _ = (evaluate_builtin ">" ((evaluate_keyword_binop ">" a b):as) vars 1)
evaluate_builtin "div" (_:0:_) _ _ = Left "Division by zero" 
evaluate_builtin "div" (a:b:as) vars _ = (evaluate_builtin "div" ((evaluate_keyword_binop "div" a b):as) vars 1)
evaluate_builtin "mod" (_:0:_) _ _ = Left "Division by zero" 
evaluate_builtin "mod" (a:b:as) vars _ = (evaluate_builtin "mod" ((evaluate_keyword_binop "mod" a b):as) vars 1)
evaluate_builtin "eq?" (a:b:as) vars _ = (evaluate_builtin "eq?" ((evaluate_keyword_binop "eq?" a b):as) vars 1)
evaluate_builtin "<=" (a:b:as) vars _ = (evaluate_builtin "<=" ((evaluate_keyword_binop "<=" a b):as) vars 1)
evaluate_builtin ">=" (a:b:as) vars _ = (evaluate_builtin ">=" ((evaluate_keyword_binop ">=" a b):as) vars 1)
evaluate_builtin ">=" (a:b:_) vars _ = (evaluate_builtin ">=" [(evaluate_keyword_binop ">=" a b)] vars 1)
evaluate_builtin "-" [a] _ 0 = return ("integer", (-a))
evaluate_builtin _ [a] _ 0 = return ("integer", a)
evaluate_builtin _ [a] _ 1 = return ("integer", a)
evaluate_builtin _ _ _ _ = Left "error no keyword"


evaluate_keyword_binops :: String -> [Integer] -> [(AST, AST)] -> Integer -> Either String (String, Integer)
evaluate_keyword_binops str a vars 0 = do 
    case find_function str vars of 
        Left err -> evaluate_builtin str a vars 0
        Right value -> evaluate_function value a vars
evaluate_keyword_binops _ [a] _ 1 = return ("integer", a)

evaluate_keyword_symbol :: String -> Either String (String, Integer)
evaluate_keyword_symbol "#t" = return ("string", 1)
evaluate_keyword_symbol "#f" = return ("string", 0)
evaluate_keyword_symbol _ = Left "error unknown symbol"

find_function :: String -> [(AST, AST)] -> Either String (AST, AST)
find_function v (((BinOp (Keyword s) xs), func):vars) | v == s = return ((BinOp (Keyword s) xs), func)
                                | otherwise = find_function v vars
find_function v (((Variable name), func):vars) | v == name = return ((Variable name), func)
                                | otherwise = find_function v vars
find_function v [] = Left ("variable " ++ v ++ " is not bound.")
find_function v _ = Left ("error no function " ++ v) 

find_variable :: String -> [(AST, AST)] -> Either String AST
find_variable v ((Variable name, value):xs) | v == name = return value
                                | otherwise = find_variable v xs
find_variable v [] = Left ("variable " ++ v ++ " is not bound.")
find_variable v _ = Left ("error no variable " ++ v)

-- associe dans l'ordre un nom de variable à une valeur, et l'ajoute pour créer un env de variables temporaires
register_lambda_variables :: [AST] -> [AST] -> [(AST, AST)]
register_lambda_variables [] []         = []
register_lambda_variables (p:ps) (v:vs) = [(p, v)] ++ (register_lambda_variables ps vs)
register_lambda_variables _ _           = []

execute_lambda :: AST -> Either String (String, Integer)
execute_lambda (Lambda params body values) = evaluate [body] (register_lambda_variables params values)
execute_lambda _                         = Left "error in lambda"

evaluate :: [AST] -> [(AST, AST)] -> Either String (String, Integer)
evaluate [] _                               = Left "error empty list"
evaluate ((Error a):_) _                    = Left a
evaluate ((Num a):_) _                      = Right ("integer", a)
evaluate ((Variable a:_)) vars              = do
    value <- find_variable a vars
    res <- evaluate [value] vars
    return res
evaluate ((Define operation func):xs) vars = do
    evaluate xs ([(operation, func)] ++ vars)
evaluate ((Keyword s):_) _               = do
    evaluate_keyword_symbol s
evaluate ((BinOp (Keyword s) a ):_) vars   = do
    as' <- sequence (map (\x -> evaluate [x] vars) a)
    evaluate_keyword_binops s (map snd as') vars 0
evaluate ((If a b c):xs) vars               = do
    a' <- evaluate [a] vars
    if (snd a') == 0 then evaluate (c:xs) vars else evaluate (b:xs) vars
evaluate ((Lambda params body values):_) _      | length values == length params = execute_lambda (Lambda params body values)
                                                | otherwise                         = Left "error in lambda number of parameters"
evaluate _ _                                = Left "error"

-- foo :: (Ord b) => [(a, b)] -> [a]
-- foo xs = map fst sorted
--   where sorted = sortOn snd xs