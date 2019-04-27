module Lex where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

data Token = 
    OpenBrace 
  | CloseBrace 
  | OpenParen 
  | CloseParen 
  | SemiColon 
  | IntKeyword 
  | ReturnKeyword 
  | CharKeyword 
  | Identifier String 
  | IntLiteral Int
  deriving (Eq)

instance Show Token where
  show OpenBrace = "{"
  show CloseBrace = "}"
  show OpenParen = "("
  show CloseParen = ")"
  show SemiColon = ";"
  show IntKeyword = "INT"
  show ReturnKeyword = "RETURN"
  show CharKeyword = "CHAR"
  show (IntLiteral i) = show i
  show (Identifier id) = id

ws :: Parser String
ws = many (oneOf " \t \n")

-- A Parser combinator that skips whitespace from both sides
lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

openParen :: Parser Token
openParen = lexeme $ char '(' *> (pure OpenParen)

closeParen :: Parser Token
closeParen = lexeme $ char ')' *> (pure CloseParen)

openBrace :: Parser Token
openBrace = lexeme $ char '{' *> (pure OpenBrace)

closeBrace :: Parser Token
closeBrace = lexeme $ char '}' *> (pure CloseBrace)

semiColon :: Parser Token
semiColon = lexeme $ char ';' *> (pure SemiColon)

intKeyword :: Parser Token
intKeyword = lexeme $ string "int" *> (pure IntKeyword)

returnKeyword :: Parser Token
returnKeyword = lexeme $ string "return" *> (pure ReturnKeyword)

charKeyword :: Parser Token
charKeyword = lexeme $ string "char" *> (pure CharKeyword)

intLiteral :: Parser Token
intLiteral = lexeme (IntLiteral <$> (read <$> many1 digit))

identifier :: Parser Token
identifier = lexeme $ Identifier <$> (many1 letter)

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = (try p) <|> q

lex1 :: Parser Token
lex1 = 
         openParen
    <||> closeParen 
    <||> openBrace 
    <||> closeBrace 
    <||> semiColon 
    <||> intKeyword 
    <||> returnKeyword
    <||> charKeyword
    <||> intLiteral
    <||> identifier

-- lexer :: String -> Maybe [Token]
-- lexer s = case x of Left _ -> Nothing
--                     Right xs -> Just xs
--     where x = parse ((many lex1) <* eof) "" s

lexer :: String -> Either ParseError [Token]
lexer s = parse ((many lex1) <* eof) "" s