module Main where

import Control.Monad
import System.Environment
import System.IO

import qualified Lex as L
import qualified Parse as P
import qualified CodeGen as C

getFileName :: [String] -> String
getFileName (x:xs) = x
getFileName [] = error "No input file provided"

main :: IO ()
main = do
  args <- getArgs
  withFile (getFileName args) ReadMode $ \handle -> do
    contents <- hGetContents handle
    mapM_ C.generate (P.parse <$> L.lexer contents)
    putStrLn "Generated assembly.s"