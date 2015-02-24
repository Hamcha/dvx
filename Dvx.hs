{-# LANGUAGE LambdaCase #-}

module Main where

import Utils (trim)
import System.Environment
import System.IO

tokenize :: [String] -> [[String]]
tokenize =
    nonempty . map (nonempty . tokenizeLine)
    where
    tokenizeLine = splitAndKeep " ," [] . trim
    nonempty = filter (\x -> length x > 0)

splitAndKeep :: [Char] -> String -> String -> [String]
splitAndKeep _ s  []     = s : []
splitAndKeep c s (x:xs)  =
    case find c x of
        Just a  -> s : [a] : splitAndKeep c []         xs
        Nothing ->           splitAndKeep c (s ++ [x]) xs
    where
    find :: Eq a => [a] -> a -> Maybe a
    find []     _             = Nothing
    find (c:cx) x | c == x    = Just c
                  | otherwise = find cx x

main =
    getArgs >>=
    \case []     -> getProgName >>= \x -> putStr $ concat ["Usage: ", x, " <file.dvx>\n"]
          (x:xs) -> print . tokenize . lines =<< readFile x