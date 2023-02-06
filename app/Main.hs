module Main (main) where

import System.Environment
import Data.Char
import System.Exit
import System.IO
import Data.Maybe
import Text.Read

import Lib
import ArgsHandling
import Parser

print_result :: (String, Integer) -> IO ()
print_result ("string", 1) = putStrLn "#t"
print_result ("string", 0) = putStrLn "#f"
print_result (_, val) = putStrLn $ (show val)

main :: IO ()
main = do 
    argv <- getArgs
    contents <- argsHandling argv
    interpretor contents
    where
        interpretor :: String -> IO ()
        interpretor contents = do
            case parseThenEval contents of
                Left err -> putStrLn $ "Error: " ++ err
                Right val -> print_result val
                -- Right val -> putStrLn $ (show val)
