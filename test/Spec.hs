import System.Exit
import Test.HUnit
import UnitTestArgsHandling
import UnitTestParserChar
import UnitTestParserString
import UnitTestParserNumber
import UnitTestParentesis
import UnitTestLambda
import UnitTestDefine
import UnitTestList
import UnitTestVariable
import UnitTestBinop
import UnitTestIf

runAllTests :: IO Counts
runAllTests = runTestTT $ TestList [
    TestLabel "args handling" argsTestsList,
    TestLabel "char" charTestList,
    TestLabel "string" stringTestList,
    TestLabel "number" numberTestList,
    TestLabel "parentesis" parentesisTestList,
    TestLabel "lambda" lambdaTestList,
    TestLabel "define" defineTestList,
    TestLabel "list" listTestList,
    TestLabel "variable" variableTestList,
    TestLabel "binop" binopTestList,
    TestLabel "if" ifTestList]

main :: IO Counts
main = do
    counts <- runAllTests
    if (errors counts + failures counts) > 0
        then exitFailure
        else exitSuccess
