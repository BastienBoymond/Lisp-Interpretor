module UnitTestParserNumber where

import Test.HUnit
import Parser
import Utils

numberGenericTest :: String -> Integer -> String -> Test
numberGenericTest str target left = TestCase (
    assertEqual "Basic number test" expected res)
    where
        expected = (target, left)
        res = getNbFromAST (runParser number str)

numberGenericTestFail :: String -> Test
numberGenericTestFail str = TestCase (
    assertEqual "Basic number test" True res)
    where
        res = isLeft (runParser number str)

numberTest1 :: Test
numberTest1 = numberGenericTest "1234" 1234 ""

numberTest2 :: Test
numberTest2 = numberGenericTest "1" 1 ""

numberTest3 :: Test
numberTest3 = numberGenericTest "0" 0 ""

numberTest4 :: Test
numberTest4 = numberGenericTest "1234abc" 1234 "abc"

numberTest5 :: Test
numberTest5 = numberGenericTest "1234 1234" 1234 " 1234"

numberTest6 :: Test
numberTest6 = numberGenericTest "1234é" 1234 "é"

numberTest7 :: Test
numberTest7 = numberGenericTest "1234 1234" 1234 " 1234"

numberTest8 :: Test
numberTest8 = numberGenericTestFail "" 

numberTest9 :: Test
numberTest9 = numberGenericTestFail "abc"

numberTest10 :: Test
numberTest10 = numberGenericTestFail " "

numberTest11 :: Test
numberTest11 = numberGenericTestFail "b123"

numberTestList :: Test
numberTestList = TestList [
    TestLabel "number test 1" numberTest1,
    TestLabel "number test 2" numberTest2,
    TestLabel "number test 3" numberTest3,
    TestLabel "number test 4" numberTest4,
    TestLabel "number test 5" numberTest5,
    TestLabel "number test 6" numberTest6,
    TestLabel "number test 7" numberTest7,
    TestLabel "number test 8" numberTest8,
    TestLabel "number test 9" numberTest9,
    TestLabel "number test 10" numberTest10,
    TestLabel "number test 11" numberTest11]
