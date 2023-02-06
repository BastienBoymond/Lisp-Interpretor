module UnitTestDefine where

import Test.HUnit
import Parser

getParsingLeft :: String -> Maybe String
getParsingLeft str = case runParser isdefine str of
    Left _ -> Nothing
    Right (_, b) -> Just b

isDefineMatch :: String -> Test
isDefineMatch str = TestCase (
    assertEqual "Basic define test" True res)
    where
        res = getParsingLeft str == Just ""

isDefineNotMatch :: String -> Test
isDefineNotMatch str = TestCase (
    assertEqual "Basic define test" False res)
    where
        res = getParsingLeft str == Just ""

defineTest1 :: Test
defineTest1 = isDefineMatch "define x 1"

defineTest2 :: Test
defineTest2 = isDefineMatch "define x (+ 1 2)"

defineTest3 :: Test
defineTest3 = isDefineMatch "define x (lambda (x) x)"

defineTest4 :: Test
defineTest4 = isDefineMatch "define x (lambda (x y) (+ x y))"

defineTest5 :: Test
defineTest5 = isDefineMatch "define x (lambda (x y z) (+ x y z))"

defineTest6 :: Test
defineTest6 = isDefineNotMatch "define x (lambda (x y z) (+ x y z)"

defineTest7 :: Test
defineTest7 = isDefineMatch "define x (lambda (x y) (+ x y z))"

defineTest8 :: Test
defineTest8 = isDefineNotMatch "define x (lambda (x y) (+ x y z)"

defineTestList :: Test
defineTestList = TestList [
    TestLabel "defineTest1" defineTest1,
    TestLabel "defineTest2" defineTest2,
    TestLabel "defineTest3" defineTest3,
    TestLabel "defineTest4" defineTest4,
    TestLabel "defineTest5" defineTest5,
    TestLabel "defineTest6" defineTest6,
    TestLabel "defineTest7" defineTest7]
