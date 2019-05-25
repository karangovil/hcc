module CodeGen where

import Control.Monad.State
import Data.Char
import Numeric
import qualified AST as AST

counter :: State Int Int
counter = do
      x <- get
      put (x+1)
      return x

----------------------------------------------------------------------------
-- generate assembly for expressions
----------------------------------------------------------------------------
generateComparison :: String -> String
generateComparison instruction = "    cmpl    %ecx, %eax\n" ++
                                 "    movl    $0, %eax\n" ++
                                 "    " ++ instruction ++ "    %al\n"

generateBinOp :: AST.BinaryOp -> String
generateBinOp op = case op of
            AST.Div          -> "    xor     %edx, %edx\n" ++
                                "    idivl   %ecx\n"
            AST.Mod          -> "    xor     %edx, %edx\n" ++
                                "    idivl   %ecx\n" ++
                                "    movl    %edx, %eax\n"
            AST.Add          -> "    addl    %ecx, %eax\n"
            AST.Mult         -> "    imul    %ecx, %eax\n"
            AST.Sub          -> "    subl    %ecx, %eax\n"
            AST.Equal        -> generateComparison "sete"
            AST.NotEqual     -> generateComparison "setne"
            AST.Less         -> generateComparison "setl"
            AST.LessEqual    -> generateComparison "setle"
            AST.Greater      -> generateComparison "setg"
            AST.GreaterEqual -> generateComparison "setge"

generateExpressionASM :: AST.Expr -> State Int String

generateExpressionASM (AST.BinOpExpr AST.Or e1 e2) = do
                e1ASM <- generateExpressionASM e1
                n <- counter
                e2ASM <- generateExpressionASM e2
                return $
                  e1ASM ++
                  "    cmpl    $0, %eax\n" ++
                  "    je      _or_clause_" ++ show n ++ "\n" ++
                  "    movl    $1, %eax\n" ++
                  "    jmp     _or_end_" ++ show n ++ "\n" ++
                  "_or_clause_" ++ show n ++ ":\n" ++
                  e2ASM ++
                  "    cmpl    $0, %eax\n" ++
                  "    movl    $0, %eax\n" ++
                  "    setne   %al\n" ++
                  "_or_end_" ++ show n ++ ":\n"

generateExpressionASM (AST.BinOpExpr AST.And e1 e2) = do
                e1ASM <- generateExpressionASM e1
                n <- counter
                e2ASM <- generateExpressionASM e2
                return $
                  e1ASM ++
                  "    cmpl    $0, %eax\n" ++
                  "    jne     _and_clause_" ++ show n ++ "\n" ++
                  "    movl    $1, %eax\n" ++
                  "    jmp     _and_end_" ++ show n ++ "\n" ++
                  "_and_clause_" ++ show n ++ ":\n" ++
                  e2ASM ++
                  "    cmpl    $0, %eax\n" ++
                  "    movl    $0, %eax\n" ++
                  "    setne   %al\n" ++
                  "_and_end_" ++ show n ++ ":\n"             

generateExpressionASM (AST.BinOpExpr op e1 e2) = do
                e1ASM <- generateExpressionASM e1
                e2ASM <- generateExpressionASM e2
                return $
                  -- e1 -> eax
                  e1ASM ++
                  -- eax -> stack
                  "    push    %rax\n" ++
                  -- e2 -> eax
                  e2ASM ++
                  -- eax -> ecx
                  "    movl    %eax, %ecx\n" ++
                  -- pop e1 -> eax
                  "    pop     %rax\n" ++
                  -- now e1 is in %eax and e2 is in %ecx
                  (generateBinOp op)

generateExpressionASM (AST.UnOpExpr AST.Complement expr) = do
                  exprASM <- generateExpressionASM expr
                  return $ exprASM ++ "    not    %eax\n"
generateExpressionASM (AST.UnOpExpr AST.Negation expr) = do
                  exprASM <- generateExpressionASM expr
                  return $ exprASM ++ "    neg    %eax\n"
generateExpressionASM (AST.UnOpExpr AST.Plus expr) = do
                  exprASM <- generateExpressionASM expr
                  return $ exprASM
generateExpressionASM (AST.UnOpExpr AST.Not expr) = do
                  exprASM <- generateExpressionASM expr
                  return $ 
                    exprASM ++
                    "    cmpl    $0, %eax\n" ++
                    "    movl    $0, %eax\n" ++
                    "    sete    %al\n"           
generateExpressionASM (AST.ConstExpr (AST.Int i))  = do
                    return $ "    movl    $" ++ (show i) ++ ", %eax\n"
generateExpressionASM (AST.ConstExpr (AST.Char c)) = do
                    return $ "    movl    $" ++ (show $ ord c) ++ ", %eax\n"
generateExpressionASM (AST.ConstExpr (AST.Oct o))  = do
                    return $ "    movl    $" ++ (show$ fst $ head $ readOct o) ++ ", %eax\n"
generateExpressionASM (AST.ConstExpr (AST.Hex h))  = do
                    return $ "    movl    $" ++ (show$ fst $ head $ readHex h) ++ ", %eax\n"

generateExpressionASM _ = error "Expression not supported"

generateExpression :: AST.Expr -> String
generateExpression expr = evalState (generateExpressionASM expr) 0
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