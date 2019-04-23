module CodeGen where

import System.IO
import Control.Monad
import qualified AST as AST

generateStatement :: Handle -> AST.Statement -> IO ()
generateStatement h AST.Return = do
    hPutStrLn h "ret"
generateStatement h (AST.ReturnVal (AST.Const (AST.Int i))) = do
    hPutStrLn h ("    movl    $" ++ (show i) ++ ", %eax")
    hPutStrLn h "    ret"
generateStatement h _ = error "Expression not supported"

generateFunction :: Handle -> AST.FuncDecl -> IO ()
generateFunction h (AST.FuncDecl _ (AST.Id func_name ) _ (AST.FuncBody func_body)) = do
    hPutStrLn h ("_" ++ func_name ++ ":")
    mapM_ (generateStatement h) func_body

generate :: AST.Program -> IO ()
generate (AST.Program func_decl) = do
    withFile "assembly.s" WriteMode $ \handle -> do
      generateFunction handle func_decl