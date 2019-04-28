module Main where

import System.Environment
import System.IO
import GHC.IO.Handle.Types
import Data.List.Split (splitOn)
import Data.Either

import qualified Lex as L
import qualified Parse as P
import qualified CodeGen as C

-- Safely get fileName from input args 
getFileName :: [String] -> String
getFileName (x:xs) = x
getFileName [] = error "No input file provided"

-- Extract fileName from handle with a given file extension
getOutFileNameFromHandle :: Handle -> IO String
getOutFileNameFromHandle (FileHandle fileName _) = do
    return outFileName
    where
      outFileName = if 
        elem (last $ splitOn "." fileName) ["c", "cc"] then (head $ splitOn "." fileName)
        else error "File extension must be .c or .cc"

main :: IO ()
main = do
  args <- getArgs
  withFile (getFileName args) ReadMode $ \handle -> do
    outFileName <- getOutFileNameFromHandle handle
    contents <- hGetContents handle
    --print contents
    let res = (C.generate <$> (P.parseProgram <$> L.lexer contents))
    --print res
    let result = fromRight "Failed to generate assembly" res
    writeFile (outFileName ++ ".s") result