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
    Map.insert Lex.Plus AST.Add $ Map.empty

----------------------------------------------------------------------------
-- parse a factor
-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
----------------------------------------------------------------------------
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
parseFactor ((Lex.IntLiteral i) : rest) = (AST.ConstExpr (AST.Int i), rest)
parseFactor ((Lex.CharLiteral c) : rest) = (AST.ConstExpr (AST.Char c), rest)
parseFactor ((Lex.OctLiteral o) : rest) = (AST.ConstExpr (AST.Oct o), rest)
parseFactor ((Lex.HexLiteral h) : rest) = (AST.ConstExpr (AST.Hex h), rest)
parseFactor (token : rest) = error ("Unrecognized token " ++ (show token) ++ " in parseFactor")
parseFactor [] = error "Failed to parse factor"

----------------------------------------------------------------------------
-- parse a term
-- <term> ::= <factor> { ("*" | "/") <factor> }
----------------------------------------------------------------------------
buildTerm :: AST.Expr -> [Lex.Token] -> (AST.Expr, [Lex.Token])
buildTerm left_factor toks@(binop:right)
      | elem binop [Lex.Multiply, Lex.Division, Lex.Modulo] =
          let (right_factor, rest) = parseFactor right
              left_factor' = AST.BinOpExpr (fromJust (Map.lookup binop binop_map)) left_factor right_factor
          in buildTerm left_factor' rest
      | otherwise = (left_factor, toks)
buildTerm left_factor [] = (left_factor, [])      

parseTerm :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseTerm tokens =
    let (left, rest) = parseFactor tokens
    in buildTerm left rest

----------------------------------------------------------------------------
-- parse an expression
-- <exp> ::= <term> { ("+" | "-") <term> }
----------------------------------------------------------------------------
buildExpr :: AST.Expr -> [Lex.Token] -> (AST.Expr, [Lex.Token])
buildExpr left_term toks@(binop:right)
      | elem binop [Lex.Plus, Lex.Negation] =
          let (right_term, rest) = parseTerm right
              left_term' = AST.BinOpExpr (fromJust (Map.lookup binop binop_map)) left_term right_term
              in buildExpr left_term' rest
      | otherwise = (left_term, toks)
buildExpr left_term [] = (left_term, []) 

parseExpr :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseExpr tokens =
    let (left, rest) = parseTerm tokens
    in buildExpr left rest      

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