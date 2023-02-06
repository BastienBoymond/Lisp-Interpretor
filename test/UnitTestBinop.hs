module UnitTestBinop where

import Test.HUnit
import Parser

binopTestGeneric :: String -> Test
binopTestGeneric str = TestCase (
    assertEqual "Basic binop test" True res)
    where
        res = getParsingLeft str == Just ""
        getParsingLeft str = case runParser binop str of
            Left _ -> Nothing
            Right (_, b) -> Just b

binopTest1 :: Test
binopTest1 = binopTestGeneric "+"

binopTest2 :: Test
binopTest2 = binopTestGeneric "-"

binopTest3 :: Test
binopTest3 = binopTestGeneric "*"

binopTest4 :: Test
binopTest4 = binopTestGeneric "/"

binopTest5 :: Test
binopTest5 = binopTestGeneric "mod"

binopTest6 :: Test
binopTest6 = binopTestGeneric "and"

binopTest7 :: Test
binopTest7 = binopTestGeneric "or"

binopTest8 :: Test
binopTest8 = binopTestGeneric "not"

binopTest9 :: Test
binopTest9 = binopTestGeneric "eq?"

binopTest10 :: Test
binopTest10 = binopTestGeneric "feaae"

binopTestList :: Test
binopTestList = TestList [
    TestLabel "binopTest1" binopTest1,
    TestLabel "binopTest2" binopTest2,
    TestLabel "binopTest3" binopTest3,
    TestLabel "binopTest4" binopTest4,
    TestLabel "binopTest5" binopTest5,
    TestLabel "binopTest6" binopTest6,
    TestLabel "binopTest7" binopTest7,
    TestLabel "binopTest8" binopTest8,
    TestLabel "binopTest9" binopTest9,
    TestLabel "binopTest10" binopTest10]