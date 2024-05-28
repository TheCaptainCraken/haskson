{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Data.Char (isDigit, isSpace)

data JsonValue -- NOTE: Does not support multiline JSON
  = JsonString String -- NOTE: does not support escaping
  | JsonNumber Integer -- NOTHE: no support for floating point numbers
  | JsonBool Bool
  | JsonNull
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- NOTE: no error reporting
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

-- to use the sequenceA operator
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)
  pure x = Parser $ \input -> Just (input, x)

instance Alternative Parser where
  empty = Parser $ const empty

  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

-- Creates a parser for a single character
charParser :: Char -> Parser Char
charParser x =
  Parser $ \case
    y : rest
      | y == x -> Just (rest, x)
    _ -> Nothing

-- Creates a parser for a single string
stringParser :: String -> Parser String
stringParser = traverse charParser

predicativeParser :: (Char -> Bool) -> Parser String
predicativeParser f = Parser $ \input ->
  let (token, rest) = span f input in Just (rest, token)

-- If the parsed thing is "" then we ignore it and return Nothing
purgeNull :: Parser [a] -> Parser [a]
purgeNull (Parser p) = Parser $ \input -> do
  (input', parsed) <- p input
  if null parsed then Nothing else Just (input', parsed)

stringLiteralParser :: Parser String
stringLiteralParser = charParser '"' *> predicativeParser (/= '"') <* charParser '"'

whiteSpaceParser :: Parser String
whiteSpaceParser = predicativeParser isSpace

separatorParser :: Parser a -> Parser b -> Parser [b]
separatorParser separator element = (:) <$> element <*> many (separator *> element) <|> pure []

-- If the parser is ok with it then we ignore the shit out
-- of anything is returns and return JsonNull
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringParser "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringParser "true" <|> stringParser "false")
  where
    f "true" = JsonBool True
    f "flase" = JsonBool False
    f _ = undefined -- This should never happen

jsonNumber :: Parser JsonValue
jsonNumber = f <$> purgeNull (predicativeParser isDigit)
  where
    f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteralParser

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charParser '[' *> whiteSpaceParser *> arrayElements <* whiteSpaceParser <* charParser ']')
  where
    arrayElements = separatorParser sep jsonValue
    sep = whiteSpaceParser *> charParser ',' <* whiteSpaceParser

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charParser '{' *> whiteSpaceParser *> separatorParser (whiteSpaceParser *> charParser ',' <* whiteSpaceParser) objectRecordParser <* whiteSpaceParser <* charParser '}')
  where
    objectRecordParser = (\key _ value -> (key, value)) <$> stringLiteralParser <*> (whiteSpaceParser *> charParser ':' *> whiteSpaceParser) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO ()
main = print $ runParser jsonValue "ciwpiouawifjawoief"