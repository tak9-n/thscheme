module Parser where

import Text.ParserCombinators.Parsec
import Numeric
import LispTypes

symbolLetter :: Parser Char
symbolLetter = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 = skipMany1 space

readExpr input = parse parseExpr "lisp" input

parseEscape :: Parser Char
parseEscape = do char '\\'
                 escaped <- anyChar
                 return $ case escaped of
                   't'  -> '\t'
                   'n'  -> '\n'
                   'r'  -> '\r'
                   _    -> escaped
                 
parseString :: Parser LispTypes
parseString = do char '"'
                 x <- many ( parseEscape <|> (noneOf "\"") )
                 char '"'
                 return $ String x

parseSymbol :: Parser LispTypes
parseSymbol = do first <- letter <|> symbolLetter
                 rest <- many (letter <|> digit <|> symbolLetter)
                 let sym = first:rest
                 return $ case sym of
                   "#t" -> Bool True
                   "#f" -> Bool False
                   _    -> Symbol sym

parseDecimal :: Parser Integer
parseDecimal = do decimal <- many1 digit
                  return $ read decimal

parseHex :: Parser Integer
parseHex = do  char 'x'
               hex <- many (oneOf "0123456789abcdefABCDEF")
               return $ case readHex hex of
                 [(a,_)] -> a
                 _ -> 0

parseOct :: Parser Integer
parseOct = do char 'o'
              oct <- many (oneOf "012345678")
              return $ case readOct oct of 
                [(a,_)] -> a
                _ -> 0

               
parseInteger :: Parser LispTypes
parseInteger = (parseDecimal <|> ( (char '#') >> (parseOct <|> parseHex) )) >>= \x -> (return $ Integer x)

parseFloat :: Parser LispTypes
parseFloat = do integer <- many digit
                char '.'
                decimal <- many1 digit
                return $ Float $ case (readFloat $ integer ++ "." ++ decimal) of
                  [(a,_)] -> a
                  _ -> 0.0

parseParent :: Parser LispTypes
parseParent = do char '('
                 spaces
                 content <- try parseList <|> parsePair
                 spaces
                 char ')'
                 return content

parseList :: Parser LispTypes
parseList = do lst <- (sepBy parseExpr spaces1)
               return $ List lst

parsePair :: Parser LispTypes
parsePair = do head <- parseExpr
               spaces1
               char '.'
               spaces1
               tail <- parseExpr
               return $ Pair head tail

parseQuote :: Parser LispTypes
parseQuote = do char '\''
                p <- parseExpr
                return $ List [Symbol "QUOTE" , p ]
                
parseExpr :: Parser LispTypes
parseExpr =  parseString
  <|> parseInteger
  <|> parseFloat
  <|> parseSymbol
  <|> parseQuote
  <|> parseParent
