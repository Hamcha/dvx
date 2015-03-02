module Dvx.Utils
( isNumeric
, joinstr
, joinsub
, middle
, splitAndKeep
, splitOn
, trim
) where

import Data.Char (isSpace, isDigit)
import Data.List (intercalate)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ []              = []
splitOn i lst | null b    = [a]
              | otherwise = a : splitOn i (tail b)
              where
              (a, b) = break (== i) lst -- Get before/after separator

splitAndKeep :: String -> String -> [String]
splitAndKeep _ []            = []
splitAndKeep f s | null b    = [a]
                 | otherwise = a : [head b] : splitAndKeep f (tail b)
                 where
                 (a, b) = break (`elem` f) $ trim s

isNumeric :: String -> Bool
isNumeric = all isDigit

middle :: String -> String
middle []     = error "Called middle on empty string."
middle (_:[]) = error "Called middle on length 1 string."
middle x      = tail $ init x

joinsub :: [[a]] -> [a]
joinsub = foldr (\a b -> a ++ b) []

joinstr :: [String] -> Int -> [String] -> [String]
joinstr acc depth []     | depth > 0 = error $ "unclosed string: " ++ (intercalate " " acc)
                         | otherwise = reverse acc
joinstr acc 0     (x:xs) | last x == '}' = error $ "unexpected string closing: " ++ (intercalate " " acc)
                         | head x == '{' = joinstr (x:acc) 1 xs
                         | otherwise     = joinstr (x:acc) 0 xs
joinstr acc depth (x:xs) = joinstr ((head acc ++ x):(tail acc)) (depth + nesting) xs
                           where
                           nesting = if      head x == '{' then 1
                                     else if last x == '}' then -1
                                     else                       0
