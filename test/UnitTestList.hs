module UnitTestList where

import Test.HUnit
import Parser

listTestGeneric :: String -> Test
listTestGeneric str = TestCase (
    assertEqual "Basic list test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser list str of
            Left _ -> Nothing
            Right (_, b) -> Just b

listTestGenericFailed :: String -> Test
listTestGenericFailed str = TestCase (
    assertEqual "Basic list test" False res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser list str of
            Left _ -> Nothing
            Right (_, b) -> Just b

listTest1 :: Test
listTest1 = listTestGeneric "(1 2 3)"

listTest2 :: Test
listTest2 = listTestGeneric "(1 2 3 4)"

listTest3 :: Test
listTest3 = listTestGeneric "(1 2 3 4 5)"

listTest4 :: Test
listTest4 = listTestGeneric "()"

listTest5 :: Test
listTest5 = listTestGenericFailed "(1 2 3 4 5"

listTest6 :: Test
listTest6 = listTestGenericFailed "1 2 3 4 5)"

listTest7 :: Test
listTest7 = listTestGenericFailed "(1 2 3 4 5))"

listTest8 :: Test
listTest8 = listTestGenericFailed "((1 2 3 4 5))"

listTest9 :: Test
listTest9 = listTestGeneric "(1   2   3   4   5)"

listTestList :: Test
listTestList = TestList [
    TestLabel "listTest1" listTest1,
    TestLabel "listTest2" listTest2,
    TestLabel "listTest3" listTest3,
    TestLabel "listTest4" listTest4,
    TestLabel "listTest5" listTest5,
    TestLabel "listTest6" listTest6,
    TestLabel "listTest7" listTest7,
    TestLabel "listTest8" listTest8,
    TestLabel "listTest9" listTest9]