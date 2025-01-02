import AST
import Lexer
import Data.List (partition)

type TestResult = (String, Bool)

-- Run a single lexer test
runTest :: String -> [Token] -> IO TestResult
runTest input expected =
    let result = lexer input
    in if result == expected
       then return (input, True)
       else do
           putStrLn $ "Test failed for input: " ++ show input ++ 
                       "\n   Expected: " ++ show expected ++ 
                       "\n   Actual:      " ++ show result
           return (input, False)

-- Remove duplicates in test output
uniqueResults :: [TestResult] -> [TestResult]
uniqueResults = foldr (\(input, res) acc -> if input `elem` map fst acc then acc else (input, res) : acc) []

-- Run all tests
main :: IO ()
main = do
    putStrLn "Running Lexer Tests...\n"

    testResults <- sequence [
        -- Keywords
        runTest "int" [Keyword "int"],
        runTest "if" [Keyword "if"],
        runTest "return" [Keyword "return"],

        -- Identifiers
        runTest "variable" [Identifier "variable"],
        runTest "_valid" [Identifier "_valid"],
        runTest "var123" [Identifier "var123"],

        -- Integer Literals
        runTest "123" [IntegerLiteral 123],
        runTest "0" [IntegerLiteral 0],

        -- Floating-Point Literals
        runTest "3.14" [FloatingPointLiteral 3.14],
        runTest "0.0" [FloatingPointLiteral 0.0],

        -- String Literals
        runTest "\"hello\"" [StringLiteral "hello"],
        runTest "\"escaped \\\"string\\\"\"" [StringLiteral "escaped \\\"string\\\""],
        runTest "\"unterminated" [Err "Unterminated string literal"],

        -- Boolean Literals
        runTest "true" [BoolLiteral True],
        runTest "false" [BoolLiteral False],

        -- Operators
        runTest "+" [Plus],
        runTest "==" [EqualEqual],
        runTest "!=" [NotEqual],

        -- Symbols
        runTest "(" [LPar],
        runTest ")" [RPar],
        runTest ";" [SemiColon],

        -- Comments
        runTest "// Single-line comment" [SingleLineComment " Single-line comment"],
        runTest "/* Multi-line */" [MultiLineComment " Multi-line "],
        runTest "/* Unterminated" [Err "Unterminated comment"],

        -- Mixed Valid Code
        runTest "int x = 5;" [Keyword "int", Identifier "x", Equals, IntegerLiteral 5, SemiColon],
        runTest "if (x > 0) { return; }" 
            [Keyword "if", LPar, Identifier "x", GreaterThan, IntegerLiteral 0, RPar,
             LBrace, Keyword "return", SemiColon, RBrace],

        -- Error Handling
        runTest "#" [Err "Unrecognized token: #"],
        runTest "$" [Err "Unrecognized token: $"]
        ]

    -- Ensure unique results
    let uniqueTestResults = uniqueResults testResults
    let (passedTests, failedTests) = partition snd uniqueTestResults

    putStrLn "\nPassed Tests:"
    mapM_ (putStrLn . (" - " ++) . fst) passedTests

    putStrLn "\nFailed Tests:"
    mapM_ (putStrLn . (" - " ++) . fst) failedTests

    let totalTests = length uniqueTestResults
    let passedCount = length passedTests
    let failedCount = length failedTests

    putStrLn $ "\nSummary: " ++ show passedCount ++ " / " ++ show totalTests ++ " tests passed."
