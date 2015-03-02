module Main where

import Dvx.Parser
import Dvx.Interpreter
import Std
import System.Environment
import System.IO

data Flag = F_None | F_PrintAST

-- TODO: Support multiple flags?
process :: Flag -> [String] -> IO ()
process F_PrintAST = print                . parse . tokenize
process _          = execute [stdContext] . parse . tokenize
--process = print . parse . tokenize

main :: IO ()
main = getArgs >>= parseFlags F_None

parseFlags :: Flag -> [String] -> IO ()
parseFlags _ []        = getProgName >>= \x -> putStr $ concat ["Usage: ", x, " [-A] <file.dvx>\n"]
parseFlags f ("-" :_ ) = isEOF       >>= parseStdin f []
parseFlags _ ("-A":xs) = parseFlags F_PrintAST xs
parseFlags f (x   :_ ) = readFile x >>= process f . lines

parseStdin :: Flag -> [String] -> Bool -> IO ()
parseStdin f x True  = process f x
parseStdin f x False = getLine >>= \y -> isEOF >>= parseStdin f (x ++ [y])
