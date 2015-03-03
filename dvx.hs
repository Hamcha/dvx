{-|
Module      : Main
Description : DVX Interpreter
-}
module Main where

import Dvx.Parser
import Dvx.Interpreter
import Std
import System.Environment
import System.IO

-- |Command line flags
data Flag = F_None | F_PrintAST

-- TODO: Support multiple flags?
-- |Processes the given list of lines depending on what flag is given
-- |"-A" will print the AST instead of executing
process :: Flag     -- ^ Process flag
        -> [String] -- ^ Lines of code
        -> IO ()
process F_PrintAST = print                . parse . tokenize
process _          = execute [stdContext] . parse . tokenize

-- |Entry point
main :: IO ()
main = getArgs >>= parseFlags F_None

-- |Parses command line flags
parseFlags :: Flag -> [String] -> IO ()
parseFlags _ []        = getProgName >>= \x -> putStr $ concat ["Usage: ", x, " [-A] <file.dvx>\n"]
parseFlags f ("-" :_ ) = isEOF       >>= parseStdin f []
parseFlags _ ("-A":xs) = parseFlags F_PrintAST xs
parseFlags f (x   :_ ) = readFile x >>= process f . lines

-- |Reads lines of code from stdin
parseStdin :: Flag -> [String] -> Bool -> IO ()
parseStdin f x True  = process f x
parseStdin f x False = getLine >>= \y -> isEOF >>= parseStdin f (x ++ [y])
