module Parse where

import qualified Lex as Lex
import qualified AST as AST

parseFunctionParams :: [Lex.Token] -> ([AST.FuncParam], [Lex.Token])
parseFunctionParams (Lex.CloseParen : rest) = ([], rest)

parseFunctionParams (Lex.IntKeyword : Lex.Identifier name : rest) = 
    let (other_params, rest') = parseFunctionParams rest
    in  (AST.FuncParam AST.IntType (AST.Id name) : other_params, rest')
                                                                  
parseFunctionParams (Lex.CharKeyword : Lex.Identifier name : rest) = 
    let (other_params, rest') = parseFunctionParams rest
    in  (AST.FuncParam AST.CharType (AST.Id name) : other_params, rest')
                                                                  
parseFunctionParams _ = error "Parse error in parseFunctionParams"

parseExpr :: [Lex.Token] -> (AST.Expr, [Lex.Token])
parseExpr ((Lex.IntLiteral i) : rest) = (AST.Const (AST.Int i), rest)
parseExpr ((Lex.CharLiteral c) : rest) = (AST.Const (AST.Char c), rest)
parseExpr ((Lex.OctLiteral o) : rest) = (AST.Const (AST.Oct o), rest)
parseExpr ((Lex.HexLiteral h) : rest) = (AST.Const (AST.Hex h), rest)
parseExpr (token : rest) = error ("Unrecognized token " ++ (show token) ++ " in parseExpr")
parseExpr [] = error "Expected expression in parseExpr but found none"

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

parseFunctionBody :: [Lex.Token] -> AST.FuncBody
parseFunctionBody tokens
    | rest == [Lex.CloseBrace] = AST.FuncBody statements
    | otherwise = error "Expected closing brace"
    where
      (statements, rest) = parseStatementList tokens

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

parseProgram :: [Lex.Token] -> AST.Program
parseProgram tokens = AST.Program (parseFunction tokens)