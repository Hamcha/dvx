module Dvx.Parser
( tokenize
, parseLine
) where

import Dvx.Tokens (token, DvxTokName(..))
import Dvx.Romans
import Dvx.Utils (trim, splitAndKeep, isNumeric, middle)

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
parseValue (c:[]) -- separator, no semantic value
                  | c `elem` separators              = DvxToken (token [c], Nothing)
	          -- an identifier
                  | otherwise                        = DvxToken (token [c], lookupSymbol [c])
parseValue x      -- a number literal
                  | head x == '\'' && last x == '\'' = DvxToken (NUMBER   , Just $ DvxInt $ rtod $ middle x)
	          -- a string literal
                  | head x == '{' && last x == '}'   = DvxToken (STRING   , Just $ DvxString $ middle x)
	          -- either a keyword or an identifier
	          | otherwise                        = DvxToken (token x  , lookupSymbol x)

tokenize :: [String] -> [[String]]
tokenize =
    nonempty . map (nonempty . tokenizeLine)
    where
    tokenizeLine = splitAndKeep separators [] . trim
    nonempty = filter (not . null)


-- Uhm...how are we going to keep a symbol table?
lookupSymbol :: String -> Maybe DvxValue
lookupSymbol x = Just $ DvxString x  -- TODO
