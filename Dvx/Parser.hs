module Dvx.Parser
( tokenize
, parseLine
) where

import Dvx.Tokens (token, DvxTokName(..))
import Dvx.Romans
import Dvx.Utils (trim, splitAndKeep, isNumeric)

data DvxExpr  = DvxToken  (DvxTokName, Maybe DvxValue) 
              | DvxList   [DvxExpr] 
	      deriving Show

data DvxValue = DvxInt    Int 
              | DvxString String
	     -- | DvxFunc   (String, DvxList)
	      deriving Show

separators = " ,.!:"

-- |Destructures an array of Strings in a Dvx expression
parseLine :: [String] -> DvxExpr
parseLine []     = DvxList  []
parseLine (x:[]) = parseValue x
parseLine (x:xs) = DvxList  (parseValue x : [parseLine xs])

-- |Given a String, returns the corresponding token with its semantic value, if any.
parseValue :: String -> DvxExpr 
parseValue (c:[]) 
             | c `elem` separators = DvxToken (token [c], Nothing)
             | otherwise           = DvxToken (token [c], Just $ DvxString [c])
parseValue x | isNumeric x         = DvxToken (NUMBER, Just $ DvxInt $ read x)
	     | otherwise           = DvxToken (STRING, Just $ DvxString x)

tokenize :: [String] -> [[String]]
tokenize =
    nonempty . map (romanize . nonempty . tokenizeLine)
    where
    tokenizeLine = splitAndKeep separators [] . trim
    nonempty = filter (not . null)
