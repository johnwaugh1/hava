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
        runTest "class Test {
                    public static void main(String[] args) {
                    System.out.println("My First Java Program.");
                    }
                };"
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
