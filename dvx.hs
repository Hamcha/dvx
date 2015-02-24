{-# LANGUAGE LambdaCase #-}

module Main where

import Dvx.Parser
import System.Environment

main :: IO ()
main =
    getArgs >>=
    \case []    -> getProgName >>= \x -> putStr $ concat ["Usage: ", x, " <file.dvx>\n"]
          (x:_) -> print . tokenize . lines =<< readFile x