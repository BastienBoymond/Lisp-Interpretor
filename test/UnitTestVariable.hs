module UnitTestVariable where

import Test.HUnit
import Parser

variableTestGeneric :: String -> Test
variableTestGeneric str = TestCase (
    assertEqual "Basic variable test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser variable str of
            Left _ -> Nothing
            Right (_, b) -> Just b

variableTestGenericFailed :: String -> Test
variableTestGenericFailed str = TestCase (
    assertEqual "Basic variable test" False res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser variable str of
            Left _ -> Nothing
            Right (_, b) -> Just b

variableTest1 :: Test
variableTest1 = variableTestGeneric "a"

variableTest2 :: Test
variableTest2 = variableTestGeneric "abc"

variableTest3 :: Test
variableTest3 = variableTestGeneric "abc123"

variableTest4 :: Test
variableTest4 = variableTestGeneric "abc123def"

variableTest5 :: Test
variableTest5 = variableTestGeneric "abc123def456"

variableTest6 :: Test
variableTest6 = variableTestGeneric "abc123def456ghi"

variableTest7 :: Test
variableTest7 = variableTestGeneric "abc123def456ghi789"

variableTest8 :: Test
variableTest8 = variableTestGenericFailed ")123abc123def456ghi789"

variableTestList :: Test
variableTestList = TestList [
    TestLabel "variableTest1" variableTest1,
    TestLabel "variableTest2" variableTest2,
    TestLabel "variableTest3" variableTest3,
    TestLabel "variableTest4" variableTest4,
    TestLabel "variableTest5" variableTest5,
    TestLabel "variableTest6" variableTest6,
    TestLabel "variableTest7" variableTest7,
    TestLabel "variableTest8" variableTest8]