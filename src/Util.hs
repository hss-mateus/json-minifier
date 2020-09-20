module Util where

import Prelude hiding (span)
import qualified Prelude as P
import Data.Char (isSpace)
import Parser
import Control.Applicative

-- An elementary char parser, which can be used for more complex parsers
char :: Char -> Parser Char
char c = Parser p
  where
    p (x:xs) | c == x    = Just (xs, c)
             | otherwise = Nothing
    p _ = Nothing

-- The string parser works by running the char parser sequentially
string :: String -> Parser String
string = traverse char

-- Helper function to parse until the given predicate returns false
span :: (Char -> Bool) -> Parser String
span f = Parser $ \s -> let (s', rest) = P.span f s
                        in Just (rest, s')

-- For strings surrounded by double quotes
stringLiteral :: Parser String
stringLiteral = surroundByC '"' (span (/= '"'))

-- For non-empty values
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \s ->
             case p s of
               Just (_, []) -> Nothing
               res -> res

-- For a sequence of whitespaces
whitespace :: Parser String
whitespace = span isSpace

-- Creates a parser for a value surrounded by a character
surroundByC :: Char -> Parser a -> Parser a
surroundByC c p = char c *> p <* char c

-- Same as above, but for different characters in the left and right
surroundByC2 :: (Char, Char) -> Parser a -> Parser a
surroundByC2 (l,r) p = char l *> p <* char r

-- For a value surrounded by another parser criteria
surroundByP :: Parser a -> Parser b -> Parser b
surroundByP p' p = p' *> p <* p'

-- Same but for two different parsers in the left and right
surroundByP2 :: Parser a -> Parser b -> Parser c -> Parser c
surroundByP2 pl pr p = pl *> p <* pr

-- Split elements by a separator
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = liftA2 (:) element (many $ sep *> element) <|> pure []
