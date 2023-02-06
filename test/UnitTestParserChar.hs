module UnitTestParserChar where

import Test.HUnit
import Parser
import Utils

charTestGeneric :: String -> Char -> String -> Test
charTestGeneric str c other = TestCase (
    assertEqual "Basic char test" (Right (c, other)) (runParser (char c) str))

charTestGenericFail :: String -> Char -> Test
charTestGenericFail str c = TestCase (
    assertEqual "Basic char test" True res)
    where
        res = isLeft (runParser (char c) str)

charTest1 :: Test
charTest1 = charTestGeneric "abc" 'a' "bc"

charTest2 :: Test
charTest2 = charTestGeneric "bac" 'b' "ac"

charTest3 :: Test
charTest3 = charTestGeneric "cab" 'c' "ab"

charTest4 :: Test
charTest4 = charTestGeneric "a" 'a' ""

charTest5 :: Test
charTest5 = charTestGeneric " " ' ' ""

charTest6 :: Test
charTest6 = charTestGeneric "1234" '1' "234"

charTest7 :: Test
charTest7 = charTestGeneric "é" 'é' ""

charTest8 :: Test
charTest8 = charTestGenericFail "abc" 'b'

charTest9 :: Test
charTest9 = charTestGenericFail "abc" 'c'

charTest10 :: Test
charTest10 = charTestGenericFail "abc" 'd'

charTest11 :: Test
charTest11 = charTestGenericFail "" 'a'

charTest12 :: Test
charTest12 = charTestGenericFail "" ' '

charTest13 :: Test
charTest13 = charTestGenericFail "" 'é'

charTest14 :: Test
charTest14 = charTestGenericFail "" 'è'

charTestList :: Test
charTestList = TestList [
    TestLabel "char test 1" charTest1,
    TestLabel "char test 2" charTest2,
    TestLabel "char test 3" charTest3,
    TestLabel "char test 4" charTest4,
    TestLabel "char test 5" charTest5,
    TestLabel "char test 6" charTest6,
    TestLabel "char test 7" charTest7,
    TestLabel "char test 8" charTest8,
    TestLabel "char test 9" charTest9,
    TestLabel "char test 10" charTest10,
    TestLabel "char test 11" charTest11,
    TestLabel "char test 12" charTest12,
    TestLabel "char test 13" charTest13,
    TestLabel "char test 14" charTest14]
