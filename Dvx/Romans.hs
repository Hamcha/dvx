module Dvx.Romans
( dtor
, rtod
, romanize
) where

dtor :: Int -> String
dtor 0 = "ZERO"
dtor x = show x
{- TODO
dtor x
    | x > 1000 = 'M' : dtor $ x - 1000
    | x > 100  = puppa
-}

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
    | all (`elem` "MDCLXVI") x = (show $ rtod x) : (romanize xs)
    | otherwise                =  x : romanize xs
