module Dvx.Romans 
( dtor
, rtod
, romanize
) where

dtor :: Int -> String
dtor 0 = "ZERO"
dtor x = show x

rtod :: String -> Int
rtod ""     = 0
rtod "ZERO" = 0
rtod x      =
    rsum (reverse . map rconv $ x) 0 0
    where
    rsum :: [Int] -> Int -> Int -> Int
    rsum [] tot _       = tot
    rsum (x:xs) tot max = if x > max then
                              rsum xs (tot + x) x
                          else
                              rsum xs (tot - x) max
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
romanize (x:xs) = if all (`elem` "MDCLXVI") x then
                      (show . rtod $ x) : (romanize xs)
                  else
                      x : romanize xs
