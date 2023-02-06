module UnitTestIf where

import Test.HUnit
import Parser

ifTestGeneric :: String -> Test
ifTestGeneric str = TestCase (
    assertEqual "Basic if test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser isif str of
            Left _ -> Nothing
            Right (_, b) -> Just b

ifTestGenericFail :: String -> Test
ifTestGenericFail str = TestCase (
    assertEqual "Basic if test" True res)
    where
        res = getParsingLeft str == Nothing
        getParsingLeft str = case runParser isif str of
            Left _ -> Nothing
            Right (_, b) -> Just b

ifTest1 :: Test
ifTest1 = ifTestGeneric "if #t 1 2"

ifTest2 :: Test
ifTest2 = ifTestGeneric "if #f 1 2"

ifTest3 :: Test
ifTest3 = ifTestGenericFail "if #t 1"

ifTest4 :: Test
ifTest4 = ifTestGeneric "if    #t  1   2"

ifTest5 :: Test
ifTest5 = ifTestGenericFail "of"

ifTest6 :: Test
ifTest6 = ifTestGeneric "if (eq? 1 2) 1 2"

ifTest7 :: Test
ifTest7 = ifTestGeneric "if (eq? 1 2) 1 (if #t 2 3)"

ifTest8 :: Test
ifTest8 = ifTestGeneric "if (eq? 1 2) 1 (if #t 2 (if #f 3 4))"

ifTest9 :: Test
ifTest9 = ifTestGenericFail "if"

ifTestList :: Test
ifTestList = TestList [
    TestLabel "ifTest1" ifTest1,
    TestLabel "ifTest2" ifTest2,
    TestLabel "ifTest3" ifTest3,
    TestLabel "ifTest4" ifTest4,
    TestLabel "ifTest5" ifTest5,
    TestLabel "ifTest6" ifTest6,
    TestLabel "ifTest7" ifTest7,
    TestLabel "ifTest8" ifTest8,
    TestLabel "ifTest9" ifTest9]
