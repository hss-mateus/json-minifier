module Json where

import Control.Applicative
import Data.Char
import Data.List
import Parser
import qualified Util as U

-- AST representation
data JValue = JNull
            | JBool Bool
            | JNumber Integer
            | JString String
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Show, Eq)

jNull :: Parser JValue
jNull = JNull <$ U.string "null"

jBool :: Parser JValue
jBool = JBool . f <$> (U.string "true" <|> U.string "false")
  where
    f "true" = True
    f _      = False

jNumber :: Parser JValue
jNumber = JNumber . read <$> U.notNull (U.span isDigit)

jString :: Parser JValue
jString = JString <$> U.stringLiteral

-- An array is a sequence of values, surrounded by brackets, and separated by
-- commas
jArray :: Parser JValue
jArray = JArray
  <$> U.surroundByC2 ('[',']') (U.surroundByP U.whitespace elements)
  where
    -- Intermediate parsers to separate values
    elements = U.sepBy sep jValue
    -- Elements can have whitespaces around
    sep      = U.surroundByP U.whitespace $ U.char ','

-- An object is a collection of key-value pairs, surrounded by curly braces
jObject :: Parser JValue
jObject = JObject
  <$> U.surroundByC2 ('{','}') (U.surroundByP U.whitespace entries)
  where
    entries   = U.sepBy sep jsonEntry
    sep       = U.surroundByP U.whitespace $ U.char ','
    -- An entry is a string literal and a value, separated by a colon
    jsonEntry = liftA3 (\k _ v -> (k, v))
                       U.stringLiteral
                       (U.surroundByP U.whitespace $ U.char ':')
                       jValue

jValue :: Parser JValue
jValue = jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject

-- Helper function to transform an AST representation to string. By default, this
-- function don't add any line breaks or tab spacing, so the output will be
-- always a minified version.
toString :: JValue -> String
toString val =
  case val of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JNumber n -> show n
    JString s -> surround '"' s
    JArray xs -> surround2 ('[',']') $ intercalate "," (map toString xs)
    JObject es -> surround2 ('{','}') $ intercalate "," (map entryToString es)
  where
    entryToString (k, v) = surround '"' k ++ ":" ++ toString v
    surround c x = c : x ++ [c]
    surround2 (l, r) x = l : x ++ [r]
