module Lex where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

data Token
  = OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | SemiColon
  | Plus
  | Negation
  | Multiply
  | Division
  | Modulo
  | BitComplement
  | Not
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | IntKeyword
  | ReturnKeyword
  | CharKeyword
  | Identifier String
  | IntLiteral Int
  | CharLiteral Char
  | OctLiteral String
  | HexLiteral String
  deriving (Eq, Ord)

instance Show Token where
  show OpenBrace        = "{"
  show CloseBrace       = "}"
  show OpenParen        = "("
  show CloseParen       = ")"
  show SemiColon        = ";"
  show Negation         = "-"
  show Plus             = "+"
  show Multiply         = "*"
  show Division         = "/"
  show Modulo           = "%"
  show BitComplement    = "~"
  show Not              = "!"
  show And              = "&&"
  show Or               = "||"
  show Equal            = "=="
  show NotEqual         = "!="
  show LessThan         = "<"
  show LessThanEqual    = "<="
  show GreaterThan      = ">"
  show GreaterThanEqual = ">="
  show IntKeyword       = "INT"
  show ReturnKeyword    = "RETURN"
  show CharKeyword      = "CHAR"
  show (IntLiteral i)   = show i
  show (CharLiteral c)  = show c
  show (OctLiteral o)   = show o
  show (HexLiteral h)   = show h
  show (Identifier id)  = id

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

plus :: Parser Token
plus = lexeme $ char '+' *> (pure Plus)

multiply :: Parser Token
multiply = lexeme $ char '*' *> (pure Multiply)

division :: Parser Token
division = lexeme $ char '/' *> (pure Division)

modulo :: Parser Token
modulo = lexeme $ char '%' *> (pure Modulo)

negation :: Parser Token
negation = lexeme $ char '-' *> (pure Negation)

bitComplement :: Parser Token
bitComplement = lexeme $ char '~' *> (pure BitComplement)

not' :: Parser Token
not' = lexeme $ char '!' *> (pure Not)

and' :: Parser Token
and' = lexeme $ string "&&" *> (pure And)

or' :: Parser Token
or' = lexeme $ string "||" *> (pure Or)

equal' :: Parser Token
equal' = lexeme $ string "==" *> (pure Equal)

notEqual :: Parser Token
notEqual = lexeme $ string "!=" *> (pure NotEqual)

lessThan :: Parser Token
lessThan = lexeme $ char '<' *> (pure LessThan)

lessThanEqual :: Parser Token
lessThanEqual = lexeme $ string "<=" *> (pure LessThanEqual)

greaterThan :: Parser Token
greaterThan = lexeme $ char '>' *> (pure GreaterThan)

greaterThanEqual :: Parser Token
greaterThanEqual = lexeme $ string ">=" *> (pure GreaterThanEqual)

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
  <||>  plus
  <||>  negation
  <||>  multiply
  <||>  division
  <||>  modulo
  <||>  bitComplement
  <||>  not'
  <||>  and'
  <||>  or'
  <||>  equal'
  <||>  notEqual
  <||>  lessThanEqual
  <||>  lessThan
  <||>  greaterThanEqual
  <||>  greaterThan
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