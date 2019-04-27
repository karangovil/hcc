module CodeGen where

import System.IO
import Control.Monad
import qualified AST as AST

generateStatement :: AST.Statement -> String
generateStatement AST.Return = "ret"
generateStatement (AST.ReturnVal (AST.Const (AST.Int i))) = "    movl    $" ++ (show i) ++ ", %eax \n    ret"
generateStatement _ = error "Expression not supported"

generateFunction :: AST.FuncDecl -> String
generateFunction (AST.FuncDecl _ (AST.Id func_name ) _ (AST.FuncBody func_body)) = 
                  "_" ++ func_name ++ ": \n" ++ (concatMap generateStatement func_body)

generate :: AST.Program -> String
generate (AST.Program func_decl) = "    .globl _main\n" ++ (generateFunction func_decl)