module Parser where

import Control.Applicative

-- An parser is a function from a string input to a pair, with the parsed value
-- and the rest of not parsed input
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (s', x) <- p s
    pure (s', f x)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (s, a)
  (<*>) (Parser f) (Parser p) = Parser $ \s -> do
    (s', f') <- f s
    (s'', a) <- p s'
    pure (s'', f' a)

-- The <|> operator returns the first successful operation, from left to right
instance Alternative Parser where
  empty = Parser (const empty)
  (<|>) (Parser p) (Parser q) = Parser $ liftA2 (<|>) p q
