module UnitTestParserString where

import Test.HUnit
import Parser
import Utils

stringGenericTest :: String -> String -> String -> Test
stringGenericTest str target left = TestCase (
    assertEqual "Basic string test" (Right (target, left)) (runParser (string target) str))

stringGenericTestFail :: String -> String -> Test
stringGenericTestFail str target = TestCase (
    assertEqual "Basic string test" True res)
    where
        res = isLeft (runParser (string target) str)

stringTest1 :: Test
stringTest1 = stringGenericTest "abc" "abc" ""

stringTest2 :: Test
stringTest2 = stringGenericTest "bac" "bac" ""

stringTest3 :: Test
stringTest3 = stringGenericTest "cab" "cab" ""

stringTest4 :: Test
stringTest4 = stringGenericTest "a" "a" ""

stringTest5 :: Test
stringTest5 = stringGenericTest " " " " ""

stringTest6 :: Test
stringTest6 = stringGenericTest "1234" "1234" ""

stringTest7 :: Test
stringTest7 = stringGenericTest "é" "é" ""

stringTest8 :: Test
stringTest8 = stringGenericTestFail "abc" "bac"

stringTest9 :: Test
stringTest9 = stringGenericTestFail "abc" "cab"

stringTest10 :: Test
stringTest10 = stringGenericTestFail "abc" "d"

stringTest11 :: Test
stringTest11 = stringGenericTestFail "" "a"

stringTest12 :: Test
stringTest12 = stringGenericTestFail "" " "

stringTest13 :: Test
stringTest13 = stringGenericTestFail "" "é"

stringTest14 :: Test
stringTest14 = stringGenericTest "abbc" "abb" "c"

stringTest15 :: Test
stringTest15 = stringGenericTest "abbc" "" "abbc"

stringTestList :: Test
stringTestList = TestList [
    TestLabel "string test 1" stringTest1,
    TestLabel "string test 2" stringTest2,
    TestLabel "string test 3" stringTest3,
    TestLabel "string test 4" stringTest4,
    TestLabel "string test 5" stringTest5,
    TestLabel "string test 6" stringTest6,
    TestLabel "string test 7" stringTest7,
    TestLabel "string test 8" stringTest8,
    TestLabel "string test 9" stringTest9,
    TestLabel "string test 10" stringTest10,
    TestLabel "string test 11" stringTest11,
    TestLabel "string test 12" stringTest12,
    TestLabel "string test 13" stringTest13,
    TestLabel "string test 14" stringTest14,
    TestLabel "string test 15" stringTest15]
