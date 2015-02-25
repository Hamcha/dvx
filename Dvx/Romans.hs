module Dvx.Romans
( dtor
, romanize
, romanLetters
, rtod
) where

romanLetters = "MDCLXVI"

dtor :: Int -> String
dtor 0 = ""
dtor n | n >= 1000  = 'M' : (dtor $ n - 1000)
       | n >= 100   = huns !! h ++ (dtor $ n - h * 100)
       | n >= 10    = tens !! t ++ (dtor $ n - t * 10)
       | n < 0      = '-' : dtor (-n)
       | otherwise  = ones !! n
       where
       huns = ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
       h = n `div` 100
       tens = ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
       t = n `div` 10
       ones = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]

rtod :: String -> Int
rtod ""     = 0
rtod "ZERO" = 0
rtod n      =
    rsum (reverse $ map rconv n) 0
    where
    rsum :: [Int] -> Int -> Int
    rsum []     _                = 0
    rsum (x:xs) maxN | x > maxN  = x + rsum xs x
                     | otherwise = rsum xs maxN - x
    rconv 'M' = 1000
    rconv 'D' = 500
    rconv 'C' = 100
    rconv 'L' = 50
    rconv 'X' = 10
    rconv 'V' = 5
    rconv 'I' = 1
    rconv  x  = error $ "Invalid letter: " ++ [x]

romanize :: [String] -> [String]
romanize []     = []
romanize (x:xs)
    | all (`elem` romanLetters) x = (show $ rtod x) : (romanize xs)
    | otherwise                =  x : romanize xs
