module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol::Parser Char
symbol = oneOf "!#$%&|*+_?:<=>?@^_~"

readExpr::String->String
readExpr input = case parse (spaces >> symbol) "lisp" input of 
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

spaces::Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do 
  char '"'
  x<- many (noneOf "\"")
  char '"'
  return $ String x
data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

main::IO ()
main = do 
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
