{-|
Module      : Dvx.Utils
Description : Utils functions
-}
module Dvx.Utils
( boolToInt
, isNumeric
, joinstr
, joinsub
, middle
, splitAndKeep
, splitOn
, trim
) where

import Data.Char (isSpace, isDigit)
import Data.List (intercalate)

-- |Trim whitespace from a string
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- |Split a list of lists based on a single item separator
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ []              = []
splitOn i lst | null b    = [a]
              | otherwise = a : splitOn i (tail b)
              where
              (a, b) = break (== i) lst -- Get before/after separator

-- |Split a string based on multiple separators while keeping them as items
splitAndKeep :: String -> String -> [String]
splitAndKeep _ []            = []
splitAndKeep f s | null b    = [a]
                 | otherwise = a : [head b] : splitAndKeep f (tail b)
                 where
                 (a, b) = break (`elem` f) $ trim s

-- |Check if a string is an integer
isNumeric :: String -> Bool
isNumeric = all isDigit

-- |Take out the first and last character of a string
middle :: String -> String
middle []     = error "Called middle on empty string."
middle (_:[]) = error "Called middle on length 1 string."
middle x      = tail $ init x

-- |Merge a list of lists into a single list
joinsub :: [[a]] -> [a]
joinsub = foldr (\a b -> a ++ b) []

-- |Create string tokens from a list of strings
-- This is a workaround to a design fault in our tokenizer
joinstr :: [String] -> Int -> [String] -> [String]
joinstr acc 0 []                           = reverse acc
joinstr acc _ []                           = error $ "unclosed string: " ++ (intercalate [] $ reverse acc)
joinstr acc 0 (('{':x):xs) | last x == '}' = joinstr (('{':x):acc) 0 xs
                           | otherwise     = joinstr (('{':x):acc) 1 xs
joinstr acc 0 (x:xs)       | last x == '}' = error $ "unexpected string closing: " ++ (intercalate [] $ reverse acc)
                           | otherwise     = joinstr (x:acc) 0 xs
joinstr acc depth (x:xs) =
    joinstr ((head acc ++ x):(tail acc)) (depth + nesting x) xs
    where
    nesting c | head c == '{' =  1
              | last c == '}' = -1
              | otherwise     =  0

-- |1 if given True, 0 if given False
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0
