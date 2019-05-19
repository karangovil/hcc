module CodeGen where

import Data.Char
import Numeric
import qualified AST as AST

----------------------------------------------------------------------------
-- generate assembly for expressions
----------------------------------------------------------------------------
emitComparison :: String -> String
emitComparison instruction = "    cmpl    %ecx, %eax\n" ++
                             "    movl    $0, %eax\n" ++
                             "    " ++ instruction ++ "    %al\n"

emitBinOp :: AST.BinaryOp -> String
emitBinOp op = case op of
            AST.Div          -> "    xor     %edx, %edx\n" ++
                                "    idivl   %ecx\n"
            AST.Mod          -> "    xor     %edx, %edx\n" ++
                                "    idivl   %ecx\n" ++
                                "    movl    %edx, %eax\n"
            AST.Add          -> "    addl    %ecx, %eax\n"
            AST.Mult         -> "    imul    %ecx, %eax\n"
            AST.Sub          -> "    subl    %ecx, %eax\n"
            AST.Equal        -> emitComparison "sete"
            AST.NotEqual     -> emitComparison "setne"
            AST.Less         -> emitComparison "setl"
            AST.LessEqual    -> emitComparison "setle"
            AST.Greater      -> emitComparison "setg"
            AST.GreaterEqual -> emitComparison "setge"

generateExpression :: AST.Expr -> String
generateExpression (AST.BinOpExpr op e1 e2) =
                -- e1 -> eax
                (generateExpression e1) ++
                -- eax -> stack
                "    push    %rax\n" ++
                -- e2 -> eax
                (generateExpression e2) ++
                -- eax -> ecx
                "    movl    %eax, %ecx\n" ++
                -- pop e1 -> eax
                "    pop     %rax\n" ++
                -- now e1 is in %eax and e2 is in %ecx
                (emitBinOp op)

generateExpression (AST.UnOpExpr AST.Complement expr) = (generateExpression expr) ++ "    not    %eax\n"
generateExpression (AST.UnOpExpr AST.Negation expr) = (generateExpression expr) ++ "    neg    %eax\n"
generateExpression (AST.UnOpExpr AST.Plus expr) = (generateExpression expr)
generateExpression (AST.UnOpExpr AST.Not expr) = 
                                        (generateExpression expr) ++
                                        "    cmpl    $0, %eax\n" ++
                                        "    movl    $0, %eax\n" ++
                                        "    sete    %al\n"                
generateExpression (AST.ConstExpr (AST.Int i))  = "    movl    $" ++ (show i) ++ ", %eax\n"
generateExpression (AST.ConstExpr (AST.Char c)) = "    movl    $" ++ (show $ ord c) ++ ", %eax\n"
generateExpression (AST.ConstExpr (AST.Oct o))  = "    movl    $" ++ (show$ fst $ head $ readOct o) ++ ", %eax\n"
generateExpression (AST.ConstExpr (AST.Hex h))  = "    movl    $" ++ (show$ fst $ head $ readHex h) ++ ", %eax\n"

generateExpression _ = error "Expression not supported"

----------------------------------------------------------------------------
-- generate assembly for a list of statements
----------------------------------------------------------------------------
generateStatement :: AST.Statement -> String
generateStatement AST.Return = "    ret\n"
generateStatement (AST.ReturnVal expr) = (generateExpression expr) ++ (generateStatement AST.Return)

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