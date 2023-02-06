module UnitTestArgsHandling where

import Test.HUnit
import ArgsHandling
import Control.Exception
import System.Exit

checkExit :: IO a -> IO Bool
checkExit action = 
    catch (action >> pure False) 
          (\e -> if e == ExitSuccess 
                 then pure False
                 else pure True)

readStandardInputTest :: Test
readStandardInputTest = TestCase (do x <- readStandardInput
                                     assertEqual "Standard output" "" x)

myreadFileTest :: Test
myreadFileTest = TestCase (do x <- myreadFile "test/test"
                              assertEqual "File output" "je suis lisp feur\n" x)

myreadNonExistingFileTest :: Test
myreadNonExistingFileTest = TestCase (do x <- checkExit (myreadFile "test/nonExistingFile")
                                         assertEqual "Terminal output" True x)

argsTestsList :: Test
argsTestsList = TestList [
    TestLabel "Get contents of Standard output" readStandardInputTest,
    TestLabel "Get contents of the file" myreadFileTest,
    TestLabel "Testing with non existing file" myreadNonExistingFileTest]