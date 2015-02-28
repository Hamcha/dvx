module Dvx.Utils
( isNumeric
, joinsub
, middle
, splitAndKeep
, splitOn
, trim
) where

import Data.Char (isSpace, isDigit)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ []  = []
splitOn i lst = a : splitOn i bx
                where
                (a, (_:bx)) = break (== i) lst -- Get before/after separator

splitAndKeep :: String -> String -> String -> [String]
splitAndKeep _ s  []     = [s]
splitAndKeep c s (x:xs)  =
    case find c x of
        Just a  -> s : [a] : splitAndKeep c [] (trim xs)
        Nothing -> splitAndKeep c (s ++ [x]) xs
    where
    find :: Eq a => [a] -> a -> Maybe a
    find []     _             = Nothing
    find (i:ix) j | i == j    = Just j
                  | otherwise = find ix j


isNumeric :: String -> Bool
isNumeric = all isDigit

middle :: String -> String
middle []     = error "Called middle on empty string."
middle (_:[]) = error "Called middle on length 1 string."
middle x      = tail $ init x

joinsub :: [[a]] -> [a]
joinsub = foldr (\a b -> a ++ b) []