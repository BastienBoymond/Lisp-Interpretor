module ArgsHandling where

import System.Environment
import System.Exit
import System.Directory

-- Get contents of the file or standard input
readStandardInput :: IO String
readStandardInput = do
    contents <- getContents
    return contents

-- Check if the files exists and return his contents if it does
myreadFile :: String -> IO String
myreadFile x = do
    fileExists <- doesFileExist x
    if fileExists
        then do
            contents <- readFile x
            return contents
        else
            putStrLn "File does not exist" >> exitWith(ExitFailure 84)

-- TODO
-- Il faut demander à karim ce qu'on doit faire si on a pas du tout d'argument
-- exemple : ./glados


-- The argsHandling function is a simple function that takes a list of strings as input
-- and depending on the number of elements in the input list, it reads input from a file
-- or the standard input, or exits the program with an error code. It returns an IO action that
-- returns a string.
argsHandling :: [String] -> IO String
argsHandling [] = readStandardInput
argsHandling [x] = myreadFile x
argsHandling _ = exitWith(ExitFailure 84)