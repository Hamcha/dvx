{-# LANGUAGE LambdaCase #-}

module Main where

import Dvx.Parser
import System.Environment
import System.IO

main :: IO ()
main = getArgs >>=
       \case []      -> getProgName >>= \x -> putStr $ concat ["Usage: ", x, " <file.dvx>\n"]
             ("-":_) -> parseStdin ""
             (x:_)   -> print . parse . tokenize . lines =<< readFile x

parseStdin :: String -> IO ()
parseStdin x = isEOF >>=
                \case True  -> print . parse . tokenize . lines $ x
                      False -> getLine >>= \y -> parseStdin $ x ++ y
