module Main where

-- IMPORTS --

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- DATA --
data LispVal -- All Possible Lisp Tokens (Show defined)
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

-- MAIN --
main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

-- PARSER --

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"\\" <|> (string "\\\"" >> return '"'))
  char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = do
  num <- many1 digit
  return $ Number $ read num

spaces :: Parser () -- Matches (skips) spaces
spaces = skipMany1 space

symbol :: Parser Char -- Matches given string of characters
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- EVALUATOR --
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

-- SHOW --
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
