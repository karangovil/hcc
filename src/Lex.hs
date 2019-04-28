module Lex where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

data Token
  = OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | SemiColon
  | Negation
  | BitComplement
  | LogicalNegation
  | IntKeyword
  | ReturnKeyword
  | CharKeyword
  | Identifier String
  | IntLiteral Int
  | CharLiteral Char
  | OctLiteral String
  | HexLiteral String
  deriving (Eq)

instance Show Token where
  show OpenBrace       = "{"
  show CloseBrace      = "}"
  show OpenParen       = "("
  show CloseParen      = ")"
  show SemiColon       = ";"
  show Negation        = "-"
  show BitComplement   = "~"
  show LogicalNegation = "!"
  show IntKeyword      = "INT"
  show ReturnKeyword   = "RETURN"
  show CharKeyword     = "CHAR"
  show (IntLiteral i)  = show i
  show (CharLiteral c) = show c
  show (OctLiteral o)  = show o
  show (HexLiteral h)  = show h
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

negation :: Parser Token
negation = lexeme $ char '-' *> (pure Negation)

bitComplement :: Parser Token
bitComplement = lexeme $ char '~' *> (pure BitComplement)

logicalNegation :: Parser Token
logicalNegation = lexeme $ char '!' *> (pure LogicalNegation)

intKeyword :: Parser Token
intKeyword = lexeme $ string "int" *> (pure IntKeyword)

returnKeyword :: Parser Token
returnKeyword = lexeme $ string "return" *> (pure ReturnKeyword)

charKeyword :: Parser Token
charKeyword = lexeme $ string "char" *> (pure CharKeyword)

intLiteral :: Parser Token
intLiteral = lexeme (IntLiteral <$> (read <$> many1 digit))

octQuote :: Parser String
octQuote = char '\'' *> char '\\' *> char '0' *> (many1 octDigit) <* char '\''

octWithoutQuote :: Parser String
octWithoutQuote = char '0' *> (many1 octDigit)

oct :: Parser String
oct = octQuote <|> octWithoutQuote

octLiteral :: Parser Token
octLiteral = lexeme (OctLiteral <$> oct)

hexQuote :: Parser String
hexQuote = char '\'' *> char '\\' *> char 'x' *> (many1 hexDigit) <* char '\''

hexWithoutQuote :: Parser String
hexWithoutQuote = char '0' *> char 'x' *> (many1 hexDigit)

hex :: Parser String
hex = hexQuote <|> hexWithoutQuote

hexLiteral :: Parser Token
hexLiteral = lexeme (HexLiteral <$> hex)

escape :: Parser Char
escape = do
  _ <- char '\\'
  r <- oneOf "\\\"\'nrvtbf"
  return $ case r of
              '\\' -> '\\'
              '\"' -> '\"'
              '\'' -> '\''
              'n'  -> '\n'
              'r'  -> '\r'
              'v'  -> '\v'
              't'  -> '\t'
              'b'  -> '\b'
              'f'  -> '\f'

character :: Parser Char
character = escape <|> letter

charLiteral :: Parser Token
charLiteral = lexeme (CharLiteral <$> (char '\'' *> character <* char '\''))

identifier :: Parser Token
identifier = lexeme $ Identifier <$> (many1 letter)

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = (try p) <|> q

lex1 :: Parser Token
lex1 =
        openParen
  <||>  closeParen
  <||>  openBrace
  <||>  closeBrace
  <||>  semiColon
  <||>  negation
  <||>  bitComplement
  <||>  logicalNegation
  <||>  intKeyword
  <||>  returnKeyword
  <||>  charKeyword
  <||>  octLiteral -- need to put octLiteral/hexliteral before intLiteral so
  <||>  hexLiteral -- numbers starting with zero will be read os octals/hex
  <||>  intLiteral
  <||>  charLiteral
  <||>  identifier

lexer :: String -> Either ParseError [Token]
lexer s = parse ((many lex1) <* eof) "" s