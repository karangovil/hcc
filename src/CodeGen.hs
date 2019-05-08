module CodeGen where

import Data.Char
import Numeric
import qualified AST as AST

movl :: String
movl = "    movl    $"

eax :: String
eax = "%eax\n"

al :: String
al = "%al\n"

not' :: String
not' = "    not    "

neg :: String
neg = "    neg    "

cmpl :: String
cmpl = "    cmpl    "

sete :: String
sete = "    sete    "

generateExpression :: AST.Expr -> String
generateExpression (AST.ConstExpr (AST.Int i)) = movl ++ (show i) ++ ", " ++ eax
generateExpression (AST.ConstExpr (AST.Char c)) = movl ++ (show $ ord c) ++ ", " ++ eax
generateExpression (AST.ConstExpr (AST.Oct o)) = movl ++ (show$ fst $ head $ readOct o) ++ ", " ++ eax
generateExpression (AST.ConstExpr (AST.Hex h)) = movl ++ (show$ fst $ head $ readHex h) ++ ", " ++ eax
generateExpression (AST.UnOpExpr AST.Complement expr) = (generateExpression expr) ++ not' ++ eax
generateExpression (AST.UnOpExpr AST.Negation expr) = (generateExpression expr) ++ neg ++ eax
generateExpression (AST.UnOpExpr AST.Plus expr) = (generateExpression expr)
generateExpression (AST.UnOpExpr AST.LogicalNegation expr) = 
                                        (generateExpression expr) ++
                                        cmpl ++ "$0, " ++ eax ++
                                        movl ++ "0, " ++ eax ++
                                        sete ++ al

generateExpression _ = error "Expression not supported"

generateStatement :: AST.Statement -> String
generateStatement AST.Return = "    ret"
generateStatement (AST.ReturnVal expr) = (generateExpression expr) ++ (generateStatement AST.Return)

generateFunction :: AST.FuncDecl -> String
generateFunction (AST.FuncDecl _ (AST.Id func_name ) _ (AST.FuncBody func_body)) = 
                  func_name ++ ": \n" ++ (concatMap generateStatement func_body)

generate :: AST.Program -> String
generate (AST.Program func_decl) = "    .globl main\n" ++ (generateFunction func_decl)