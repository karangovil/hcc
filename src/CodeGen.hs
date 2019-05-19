module CodeGen where

import Data.Char
import Numeric
import qualified AST as AST

----------------------------------------------------------------------------
-- helper strings for elementary assembly instructions
----------------------------------------------------------------------------
movl :: String
movl = "    movl    $"

addl :: String
addl = "    addl    "

subl :: String
subl = "    subl    "

imul :: String
imul = "    imul    "

idivl :: String
idivl = "    idivl   "

eax :: String
eax = "%eax\n"

edx :: String
edx = "%edx\n"

rsp :: String
rsp = "(%rsp)\n"

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

----------------------------------------------------------------------------
-- generate assembly for expressions
----------------------------------------------------------------------------
generateExpression :: AST.Expr -> Int-> String
generateExpression (AST.BinOpExpr op e1 e2) stack_index =
                -- e1 -> eax
                (generateExpression e1 stack_index) ++
                -- eax -> rsp
                "    movl    %eax, " ++ (show stack_index) ++ rsp ++
                -- e2 -> eax
                (generateExpression e2 (stack_index-4)) ++
                -- eax -> edx
                "    movl    %eax, " ++ edx ++
                -- rsp -> eax
                "    movl    " ++ (show stack_index) ++ "(%rsp), " ++ eax ++
                -- edx -> rsp
                "    movl    %edx, " ++ (show stack_index) ++ rsp
                -- now e1 is in %eax and e2 is in stack_index(%rsp)
                ++ ln 
              where
                ln = case op of
                  AST.Add -> addl ++ (show stack_index) ++  "(%rsp), " ++ eax
                  AST.Mult -> imul ++ (show stack_index) ++  "(%rsp), " ++ eax
                  AST.Sub -> subl ++ (show stack_index) ++  "(%rsp), " ++ eax
                  AST.Div -> "    xor     %edx, %edx\n" ++
                             idivl ++ (show stack_index) ++  "(%rsp)\n"
                  AST.Mod -> "    xor     %edx, %edx\n" ++
                             idivl ++ (show stack_index) ++  "(%rsp)\n" ++
                             "    movl    %edx, " ++ eax
generateExpression (AST.ConstExpr (AST.Int i)) _ = movl ++ (show i) ++ ", " ++ eax
generateExpression (AST.ConstExpr (AST.Char c)) _ = movl ++ (show $ ord c) ++ ", " ++ eax
generateExpression (AST.ConstExpr (AST.Oct o)) _ = movl ++ (show$ fst $ head $ readOct o) ++ ", " ++ eax
generateExpression (AST.ConstExpr (AST.Hex h)) _ = movl ++ (show$ fst $ head $ readHex h) ++ ", " ++ eax
generateExpression (AST.UnOpExpr AST.Complement expr) stack_index = (generateExpression expr stack_index) ++ not' ++ eax
generateExpression (AST.UnOpExpr AST.Negation expr) stack_index = (generateExpression expr stack_index) ++ neg ++ eax
generateExpression (AST.UnOpExpr AST.Plus expr) stack_index = (generateExpression expr stack_index)
generateExpression (AST.UnOpExpr AST.Not expr) stack_index = 
                                        (generateExpression expr stack_index) ++
                                        cmpl ++ "$0, " ++ eax ++
                                        movl ++ "0, " ++ eax ++
                                        sete ++ al

generateExpression _ _ = error "Expression not supported"

----------------------------------------------------------------------------
-- generate assembly for a list of statements
----------------------------------------------------------------------------
generateStatement :: AST.Statement -> String
generateStatement AST.Return = "    ret\n"
generateStatement (AST.ReturnVal expr) = (generateExpression expr (-4)) ++ (generateStatement AST.Return)

----------------------------------------------------------------------------
-- generate assembly for a function
----------------------------------------------------------------------------
generateFunction :: AST.FuncDecl -> String
generateFunction (AST.FuncDecl _ (AST.Id func_name ) _ (AST.FuncBody func_body)) = 
                  func_name ++ ": \n" ++ (concatMap generateStatement func_body)

----------------------------------------------------------------------------
-- generate assembly for main
----------------------------------------------------------------------------
generate :: AST.Program -> String
generate (AST.Program func_decl) = "    .globl main\n" ++ (generateFunction func_decl)