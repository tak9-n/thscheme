import Text.ParserCombinators.Parsec
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

spaces1 = skipMany1 space

readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | Pair LispVal LispVal
             | Number Integer
             | String String
             | Bool Bool

parseEscape :: Parser Char
parseEscape = do char '\\'
                 escaped <- anyChar
                 return $ case escaped of
                   't'  -> '\t'
                   'n'  -> '\n'
                   'r'  -> '\r'
                   _    -> escaped
                 
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many ( parseEscape <|> (noneOf "\"") )
--                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = do number <- many1 digit
                 return $ Number $ read number

parseParent :: Parser LispVal
parseParent = do char '('
                 spaces
--                 content <- (parsePair <|> parseList)
                 content <- try parseList <|> parsePair
                 spaces
                 char ')'
                 return content

parseList :: Parser LispVal
parseList = do lst <- (sepBy parseExpr spaces1)
               return $ List lst

parsePair :: Parser LispVal
parsePair = do head <- parseExpr
               spaces1
               char '.'
               spaces1
               tail <- parseExpr
               return $ Pair head tail

parseQuote :: Parser LispVal
parseQuote = do char '\''
                p <- parseExpr
                return $ List [Atom "QUOTE" , p ]
                
parseExpr :: Parser LispVal
parseExpr = parseParent
  <|> parseString
  <|> parseNumber
  <|> parseAtom
  <|> parseQuote
