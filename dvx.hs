{-# LANGUAGE LambdaCase #-}

module Main where

import Dvx.Parser
import System.Environment
import System.IO

execute :: [String] -> IO ()
execute = print . parse . tokenize

main :: IO ()
main = getArgs >>=
       \case []      -> getProgName >>= \x -> putStr $ concat ["Usage: ", x, " <file.dvx>\n"]
             ("-":_) -> isEOF      >>= parseStdin []
             (x:_)   -> readFile x >>= execute . lines

parseStdin :: [String] -> Bool -> IO ()
parseStdin x True  = execute x
parseStdin x False = getLine >>= \y -> isEOF >>= parseStdin (x ++ [y])
