module Main where

import Json
import Parser
import System.Exit

-- Try to parse the input, and returns a minified representation
minify :: String -> Either String String
minify json =
  case runParser jValue json of
    Just (_, parsed) -> Right $ toString parsed
    _ -> Left "error during parsing file"

-- Get contents from stdin and prints the result to stdout
main :: IO ()
main = do
  json <- getContents
  case minify json of
    Right res -> putStrLn res
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
