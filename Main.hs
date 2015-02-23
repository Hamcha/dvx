module Main where

import Utils (trim)

testtext :: String
testtext = "\
\ITALIANI!               \n\
\                        \n\
\DEFINENDO L'ADDIZIONE,  \n\
\    OVE ANNA, GIORGIO   \n\
\    È PIV ANNA, GIORGIO.\n\
\                        \n\
\SCRIVO L'ADDIZIONE      \n\
\    DI XV, IV.          \n\
\"

tokenize :: [String] -> [[String]]
tokenize =
    map tokenizeLine
    where
    tokenizeLine = splitAndKeep " ," [] . trim

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

main = print $ tokenize $ lines testtext