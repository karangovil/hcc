module CodeGen where

import Data.Char
import Numeric
import qualified AST as AST

generateStatement :: AST.Statement -> String
generateStatement AST.Return = "ret"
generateStatement (AST.ReturnVal (AST.Const (AST.Int i))) = "    movl    $" ++ (show i) ++ ", %eax \n    ret"
generateStatement (AST.ReturnVal (AST.Const (AST.Char c))) = "    movl    $" ++ (show $ ord c) ++ ", %eax \n    ret"
generateStatement (AST.ReturnVal (AST.Const (AST.Oct o))) = "    movl    $" ++ (show$ fst $ head $ readOct o) ++ ", %eax \n    ret"
generateStatement (AST.ReturnVal (AST.Const (AST.Hex h))) = "    movl    $" ++ (show$ fst $ head $ readHex h) ++ ", %eax \n    ret"
generateStatement _ = error "Expression not supported"

generateFunction :: AST.FuncDecl -> String
generateFunction (AST.FuncDecl _ (AST.Id func_name ) _ (AST.FuncBody func_body)) = 
                  func_name ++ ": \n" ++ (concatMap generateStatement func_body)

generate :: AST.Program -> String
generate (AST.Program func_decl) = "    .globl main\n" ++ (generateFunction func_decl)