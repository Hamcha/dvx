module Dvx.Parser
( tokenize
, parseLine
) where

import Dvx.Tokens (token, DvxValue(..))
import Dvx.Romans
import Dvx.Utils (trim, splitAndKeep, isNumeric, middle)

data DvxExpr  = DvxToken DvxValue
              | DvxList  [DvxExpr] 
	      deriving Show

separators = " ,.!:"

-- |Destructures an array of Strings in a Dvx expression
parseLine :: [String] -> DvxExpr
parseLine []     = DvxList  []
parseLine (x:[]) = parseValue x
parseLine (x:xs) = DvxList  (parseValue x : [parseLine xs])

-- |Given a String, returns the corresponding token with its semantic value, if any.
parseValue :: String -> DvxExpr 
parseValue (c:[]) -- separator, no semantic value
                  | c `elem` separators              = DvxToken $ token [c]
	          -- an identifier
                  | otherwise                        = DvxToken $ token [c]
parseValue x      -- a number literal
                  | head x == '\'' && last x == '\'' = DvxToken $ NUMBER $ rtod $ middle x
	          -- a string literal
                  | head x == '{' && last x == '}'   = DvxToken $ STRING $ middle x
	          -- either a keyword or an identifier
	          | otherwise                        = DvxToken $ token x

tokenize :: [String] -> [[String]]
tokenize =
    nonempty . map (nonempty . tokenizeLine)
    where
    tokenizeLine = splitAndKeep separators [] . trim
    nonempty = filter (not . null)
