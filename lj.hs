module Main (main) where

import System.Environment
import Data.List

import Parser (parseString, parseFile)
import Eval (eval)
import Syntax (Term(..))

showCode :: Maybe Term -> IO ()
showCode (Just x) =  putStrLn (show x)
showCode Nothing = putStrLn "Error running code"

main = do
    args <- getArgs
    code <- readFile (args !! 0)
    let linesOfFiles = lines code
    let results = map (eval . parseString) linesOfFiles
    mapM_ showCode results
