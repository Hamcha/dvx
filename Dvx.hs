{-# LANGUAGE LambdaCase #-}

module Main where

import Utils (trim)
import System.Environment

tokenize :: [String] -> [[String]]
tokenize =
    nonempty . map (nonempty . tokenizeLine)
    where
    tokenizeLine = splitAndKeep " ," [] . trim
    nonempty = filter (not . null)

splitAndKeep :: String -> String -> String -> [String]
splitAndKeep _ s  []     = [s]
splitAndKeep c s (x:xs)  =
    case find c x of
        Just a  -> s : [a] : splitAndKeep c []         xs
        Nothing ->           splitAndKeep c (s ++ [x]) xs
    where
    find :: Eq a => [a] -> a -> Maybe a
    find []     _             = Nothing
    find (i:ix) j | i == j    = Just j
                  | otherwise = find ix j
main :: IO ()
main =
    getArgs >>=
    \case []    -> getProgName >>= \x -> putStr $ concat ["Usage: ", x, " <file.dvx>\n"]
          (x:_) -> print . tokenize . lines =<< readFile x