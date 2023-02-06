module UnitTestParentesis where

import Test.HUnit
import Parser

getParsingLeft :: String -> Maybe String
getParsingLeft str = case runParser parentesis str of
    Left _ -> Nothing
    Right (_, b) -> Just b

isParentesisMatch :: String -> Test
isParentesisMatch str = TestCase (
    assertEqual "Basic parentesis test" True res)
    where
        res = getParsingLeft str == Just ""

isParentesisNotMatch :: String -> Test
isParentesisNotMatch str = TestCase (
    assertEqual "Basic parentesis test" False res)
    where
        res = getParsingLeft str == Just ""

parentesisTest1 :: Test
parentesisTest1 = isParentesisMatch "(+ 1 2)"

parentesisTest2 :: Test
parentesisTest2 = isParentesisMatch "((+))"

parentesisTest4 :: Test
parentesisTest4 = isParentesisMatch "(3 4)"

parentesisTest5 :: Test
parentesisTest5 = isParentesisNotMatch "((+ 1 2) 3 4)"

parentesisTest6 :: Test
parentesisTest6 = isParentesisNotMatch "((+ 1 2) 3 4"

parentesisTest7 :: Test
parentesisTest7 = isParentesisNotMatch "(+ 1 2))"

parentesisTest8 :: Test
parentesisTest8 = isParentesisNotMatch "((+ 1 2) 3 4))"

parentesisTestList :: Test
parentesisTestList = TestList [
    TestLabel "parentesisTest1" parentesisTest1,
    TestLabel "parentesisTest2" parentesisTest2,
    TestLabel "parentesisTest4" parentesisTest4,
    TestLabel "parentesisTest5" parentesisTest5,
    TestLabel "parentesisTest6" parentesisTest6,
    TestLabel "parentesisTest7" parentesisTest7,
    TestLabel "parentesisTest8" parentesisTest8]
