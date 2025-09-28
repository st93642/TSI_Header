/**
 * Haskell Code Base Generator
 * Generates boilerplate code for Haskell projects
 */

function generateHaskellCodeBase() {
    return `-- Basic Haskell program

module Main where

-- Main function - entry point
main :: IO ()
main = do
    putStrLn "Hello, World!"
    putStrLn "This is a basic Haskell program."
    runApp

-- Application logic
runApp :: IO ()
runApp = do
    let version = "1.0"
    putStrLn $ "Version: " ++ version

    -- Example with lists
    let languages = ["Haskell", "Scala", "Clojure"]
    putStrLn "Languages:"
    mapM_ (putStrLn . ("  " ++)) languages

    -- Example with records
    let config = Config {
        debug = True,
        port = 8080,
        features = ["logging", "caching"]
    }

    putStrLn "Configuration:"
    putStrLn $ "  Debug: " ++ show (debug config)
    putStrLn $ "  Port: " ++ show (port config)

-- Data types
data Config = Config {
    debug :: Bool,
    port :: Int,
    features :: [String]
} deriving (Show)

-- Utility functions
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

getInfo :: (String, String)
getInfo = ("TSI Application", "1.0")

-- Example usage (uncomment to test)
-- main = do
--     putStrLn $ greet "TSI Student"
--     let (name, version) = getInfo
--     putStrLn $ name ++ " v" ++ version`;
}

module.exports = {
    generateHaskellCodeBase
};