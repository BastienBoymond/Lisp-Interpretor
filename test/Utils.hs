module Utils where

import Parser

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

assertNumber :: AST -> Integer -> Bool
assertNumber (Num a) b  | a == b = True
                        | otherwise = False
assertNumber _ _ = False

replaceASTToNumber' :: AST -> Integer
replaceASTToNumber' (Num a) = a
replaceASTToNumber' _ = 0

getNbFromAST :: Either String (AST, String) -> (Integer, String)
getNbFromAST (Right (Num a, str)) = (a, str)
getNbFromAST _ = (0, "")

getCharFromAST :: Either String (AST, String) -> (Char, String)
getCharFromAST (Right (Symbol a, str)) = (a, str)
getCharFromAST _ = (' ', "")

getStringFromAST :: Either String (AST, String) -> (String, String)
getStringFromAST (Right (Keyword a, str)) = (a, str)
getStringFromAST _ = ("", "")
