module Dvx.Parser
( tokenize
, parse
) where

import Dvx.Tokens (token, DvxValue(..))
import Dvx.Romans
import Dvx.Utils

data DvxExpr  = DvxToken DvxValue
              | DvxList  [DvxExpr]
              deriving Show

separators :: String
separators = " ,.!:;"

parse :: [DvxValue] -> [DvxExpr]
parse = joinsub . map (extract . foldr parseTokens []) . splitOn TPeriod

extract :: [DvxExpr] -> [DvxExpr]
extract []             = []
extract (DvxToken x:DvxList y:ys) = (DvxList $ DvxToken x : extract y) : extract ys
extract (DvxList x:xs) = extract x ++ extract xs
extract x              = x

parseTokens :: DvxValue -> [DvxExpr] -> [DvxExpr]
parseTokens TSemicolon lst = DvxList [] : lst
parseTokens TComma     lst = lst
parseTokens TSpace     lst = DvxList lst : []
parseTokens x          []  = DvxList [DvxToken x] : []
parseTokens x          lst = prependList (head lst) x : tail lst

prependList :: DvxExpr -> DvxValue -> DvxExpr
prependList (DvxToken x) y = DvxList [DvxToken x, DvxToken y]
prependList (DvxList  x) y = DvxList $ DvxToken y : x

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
    map parseValue . nonempty . tokenizeLine . joinsub . map stripComments
    where
    stripComments = takeWhile (/= 'U')
    tokenizeLine  = splitAndKeep separators [] . trim
    nonempty      = filter (not . null)
