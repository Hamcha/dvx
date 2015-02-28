module Dvx.Parser
( tokenize
, parse
) where

import Dvx.Tokens (token, DvxValue(..))
import Dvx.Romans
import Dvx.Utils (trim, splitAndKeep, middle)

data DvxExpr  = DvxToken DvxValue
              | DvxList  [DvxExpr]
              deriving Show

separators :: String
separators = " ,.!:;"

parse :: [DvxValue] -> [DvxExpr]
parse = foldr parseTokens []

parseTokens :: DvxValue -> [DvxExpr] -> [DvxExpr]
parseTokens TPeriod lst = DvxList [] : lst
parseTokens x       lst = prependList (head lst) x : tail lst

prependList :: DvxExpr -> DvxValue -> DvxExpr
prependList (DvxToken x) y = DvxList [DvxToken x, DvxToken y]
prependList (DvxList  x) y = DvxList $ DvxToken y : x

-- |Destructures an array of Strings in a Dvx expression
-- parseLine :: [String] -> DvxExpr
-- parseLine []     = DvxList  []
-- parseLine (x:[]) = parseValue x
-- parseLine (x:xs) = DvxList  (parseValue x : [parseLine xs])

-- |Given a String, returns the corresponding token with its semantic value, if any.
parseValue :: String -> DvxValue
parseValue (c:[]) -- separator, no semantic value
                  | c `elem` separators              = token [c]
                  -- an identifier
                  | otherwise                        = token [c]
parseValue x      -- a number literal
                  | head x == '\'' && last x == '\'' = TNumber $ rtod $ middle x
                  -- a string literal
                  | head x == '{' && last x == '}'   = TString $ middle x
                  -- either a keyword or an identifier
                  | otherwise                        = token x

tokenize :: [String] -> [DvxValue]
tokenize =
    map parseValue . nonempty . tokenizeLine . join . map stripComments
    where
    stripComments = takeWhile (/= 'U')
    tokenizeLine  = splitAndKeep separators [] . trim
    nonempty      = filter (not . null)
    join          = foldr (\a b -> a ++ b) []
