module UnitTestLambda where

import Test.HUnit
import Parser
import Utils

getParsingLeft :: String -> Maybe String
getParsingLeft str = case runParser parentesis str of
    Left _ -> Nothing
    Right (_, b) -> Just b

isLambdaMatch :: String -> Test
isLambdaMatch str = TestCase (
    assertEqual "Basic lambda test" True res)
    where
        res = getParsingLeft str == Just ""
    
isLambdaNotMatch :: String -> Test
isLambdaNotMatch str = TestCase (
    assertEqual "Basic lambda test" False res)
    where
        res = getParsingLeft str == Just ""

lambdaTest1 :: Test
lambdaTest1 = isLambdaMatch "(lambda (x) x)"

lambdaTest2 :: Test
lambdaTest2 = isLambdaMatch "(lambda (x y) (+ x y))"

lambdaTest3 :: Test
lambdaTest3 = isLambdaMatch "(lambda (x y z) (+ x y z))"

lambdaTest4 :: Test
lambdaTest4 = isLambdaNotMatch "(lambda (x y z) (+ x y z)"

lambdaTest5 :: Test
lambdaTest5 = isLambdaMatch "(lambda (x y) (+ x y z))"

lambdaTest6 :: Test
lambdaTest6 = isLambdaNotMatch "(lambda (x y) (+ x y z)"

lambdaTestList :: Test
lambdaTestList = TestList [
    TestLabel "lambdaTest1" lambdaTest1,
    TestLabel "lambdaTest2" lambdaTest2,
    TestLabel "lambdaTest3" lambdaTest3,
    TestLabel "lambdaTest4" lambdaTest4,
    TestLabel "lambdaTest5" lambdaTest5,
    TestLabel "lambdaTest6" lambdaTest6]
