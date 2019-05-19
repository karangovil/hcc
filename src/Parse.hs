module Parse where

import qualified Lex as Lex
import qualified AST as AST

import qualified Data.Map as Map
import Data.Maybe

unops :: [Lex.Token]
unops = [Lex.BitComplement, Lex.Not, Lex.Negation, Lex.Plus]

unop_map :: Map.Map Lex.Token AST.UnaryOp
unop_map =
    Map.insert Lex.BitComplement AST.Complement .
    Map.insert Lex.Not AST.Not .
    Map.insert Lex.Negation AST.Negation .
    Map.insert Lex.Plus AST.Plus $ Map.empty

binop_map :: Map.Map Lex.Token AST.BinaryOp
binop_map =    
    Map.insert Lex.Multiply AST.Mult .
    Map.insert Lex.Division AST.Div .
    Map.insert Lex.Modulo AST.Mod .
    Map.insert Lex.Negation AST.Sub .
    Map.insert Lex.Plus AST.Add .
    Map.insert Lex.Equal AST.Equal .
    Map.insert Lex.NotEqual AST.NotEqual .
    Map.insert Lex.LessThan AST.Less .
    Map.insert Lex.LessThanEqual AST.LessEqual .
    Map.insert Lex.GreaterThan AST.Greater .
    Map.insert Lex.GreaterThanEqual AST.GreaterEqual .
    Map.insert Lex.And AST.And .
    Map.insert Lex.Or AST.Or $ Map.empty

----------------------------------------------------------------------------
-- Expression parsing grammar

-- <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
-- <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
-- <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
-- <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
-- <additive-exp> ::= <term> { ("+" | "-") <term> }
-- <term> ::= <factor> { ("*" | "/") <factor> }
-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
-- <unary_op> ::= "!" | "~" | "-"
----------------------------------------------------------------------------
buildBinExpr :: ([Lex.Token] -> (AST.Expr, [Lex.Token])) -> [Lex.Token] -> [Lex.Token] -> (AST.Expr, [Lex.Token])
buildBinExpr next_level_parser ops tokens =
      let (left, rest) = next_level_parser tokens
      in builder left rest
        where
          builder left toks@(binop:right)
            | elem binop ops =
                let (right', rest) = next_level_parser right
                    left' = AST.BinOpExpr (fromJust (Map.lookup binop binop_map)) left right'
                    in builder left' rest
            | otherwise = (left, toks)
          builder left [] = (left, [])

parseFactor :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseFactor (Lex.OpenParen : factor) = (expr, rest)
            where
              (expr, rest) = case parseExpr factor of
                    (expr, Lex.CloseParen : rest) -> (expr, rest)
                    _ -> error "Syntax error: Close paren expected"
parseFactor (unop : rest)
            | elem unop unops = (AST.UnOpExpr (fromJust (Map.lookup unop unop_map)) expr, rest')
              where
                (expr, rest') = parseFactor rest
parseFactor ((Lex.IntLiteral i) : rest)  = (AST.ConstExpr (AST.Int i), rest)
parseFactor ((Lex.CharLiteral c) : rest) = (AST.ConstExpr (AST.Char c), rest)
parseFactor ((Lex.OctLiteral o) : rest)  = (AST.ConstExpr (AST.Oct o), rest)
parseFactor ((Lex.HexLiteral h) : rest)  = (AST.ConstExpr (AST.Hex h), rest)
parseFactor (token : rest) = error ("Unrecognized token " ++ (show token) ++ " in parseFactor")
parseFactor [] = error "Failed to parse factor"

parseTerm :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseTerm tokens = buildBinExpr parseFactor [Lex.Multiply, Lex.Division, Lex.Modulo] tokens

parseAdditiveExpr :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseAdditiveExpr tokens = buildBinExpr parseTerm [Lex.Plus, Lex.Negation] tokens

parseRelationalExpr :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseRelationalExpr tokens = buildBinExpr parseAdditiveExpr [Lex.LessThan, Lex.LessThanEqual, Lex.GreaterThan, Lex.GreaterThanEqual] tokens

parseEqualityExpr :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseEqualityExpr tokens = buildBinExpr parseRelationalExpr [Lex.Equal, Lex.NotEqual] tokens

parseLogicalAndExpr :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseLogicalAndExpr tokens = buildBinExpr parseEqualityExpr [Lex.And] tokens

parseExpr :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseExpr tokens = buildBinExpr parseLogicalAndExpr [Lex.Or] tokens

----------------------------------------------------------------------------
-- parse statements that start with return and contain an expression
-- <statement> ::= "return" <exp> ";"
----------------------------------------------------------------------------

parseStatementList :: [Lex.Token] -> ([AST.Statement], [Lex.Token])
parseStatementList (Lex.SemiColon : rest) = parseStatementList rest
parseStatementList (Lex.ReturnKeyword : Lex.SemiColon : rest) =
    let (other_params, rest') = parseStatementList rest
    in  (AST.Return : other_params, rest')
parseStatementList (Lex.ReturnKeyword : rest) = 
    let (exp, rest') = parseExpr rest
        (other_statements, rest'') = parseStatementList rest'
    in  ((AST.ReturnVal exp) : other_statements, rest'')
parseStatementList tokens = ([], tokens)

----------------------------------------------------------------------------
-- parse a function body enclosed by braces
----------------------------------------------------------------------------
parseFunctionBody :: [Lex.Token] -> AST.FuncBody
parseFunctionBody tokens
    | rest == [Lex.CloseBrace] = AST.FuncBody statements
    | otherwise = error "Expected closing brace"
    where
      (statements, rest) = parseStatementList tokens

----------------------------------------------------------------------------
-- parse function params enclosed by parens
----------------------------------------------------------------------------
parseFunctionParams :: [Lex.Token] -> ([AST.FuncParam], [Lex.Token])
parseFunctionParams (Lex.CloseParen : rest) = ([], rest)

parseFunctionParams (Lex.IntKeyword : Lex.Identifier name : rest) = 
    let (other_params, rest') = parseFunctionParams rest
    in  (AST.FuncParam AST.IntType (AST.Id name) : other_params, rest')
                                                                  
parseFunctionParams (Lex.CharKeyword : Lex.Identifier name : rest) = 
    let (other_params, rest') = parseFunctionParams rest
    in  (AST.FuncParam AST.CharType (AST.Id name) : other_params, rest')
                                                                  
parseFunctionParams _ = error "Parse error in parseFunctionParams"

----------------------------------------------------------------------------
-- parse the main function
-- <function> ::= "int" <id> "(" ")" "{" <statement> "}"
----------------------------------------------------------------------------
parseFunction :: [Lex.Token] -> AST.FuncDecl
parseFunction (t1:t2:t3:rest) = AST.FuncDecl return_type func_name func_params func_body
  where
    (return_type, func_name, rest') = case (t1:t2:t3:rest) of
          (Lex.IntKeyword : (Lex.Identifier name) : Lex.OpenParen : rest') -> (AST.IntType, (AST.Id name), rest')
          (Lex.CharKeyword : (Lex.Identifier name) : Lex.OpenParen : rest') -> (AST.CharType, (AST.Id name), rest')
          _ -> error "Parse error in parseFunction : Unknown return type or name"
    (func_params, rest'') = parseFunctionParams rest'
    func_body = case rest'' of
          (Lex.OpenBrace : rest''') -> parseFunctionBody rest'''
          _ -> error "Expected brace to open function body"  

----------------------------------------------------------------------------
-- parse the whole program
-- <program> ::= <function>
----------------------------------------------------------------------------
parseProgram :: [Lex.Token] -> AST.Program
parseProgram tokens = AST.Program (parseFunction tokens)