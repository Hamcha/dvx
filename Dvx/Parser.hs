module Dvx.Parser
( tokenize
, parse
) where

import Dvx.Tokens
import Dvx.Romans
import Dvx.Utils

data DvxExpr  = DvxTok   DvxToken                   -- Single token
              | DvxCall  DvxToken [DvxExpr]         -- Function call
              | DvxFunc  DvxToken [DvxExpr] DvxExpr -- Function definition
              | DvxList  [DvxExpr]                  -- List
              deriving Show

separators :: String
separators = " ,.!:;"

parse :: [DvxToken] -> [DvxExpr]
parse = joinsub . map (makeast . foldr parseTokens []) . splitOn TPeriod

makeast :: [DvxExpr] -> [DvxExpr]
makeast [] = []
-- Function definition
makeast (DvxTok TDefn
        :DvxList (DvxTok name
                 :DvxList (DvxTok TDefnArgs
                          :DvxList args
                          :DvxList expr
                          :_)
                 :_)
        :ys)
        = DvxFunc name args (makeast expr !! 0) : makeast ys
-- Nullcall (È, DI etc.)
makeast (DvxTok TNullCall  :xs) = makeast xs
-- Function call
makeast (DvxTok x:DvxList y:ys) = DvxCall x (makeast y) : makeast ys
-- List of list
makeast (DvxList x:xs) = makeast x ++ makeast xs
-- Plain token
makeast (DvxTok  x:xs) = DvxTok x : makeast xs

parseTokens :: DvxToken -> [DvxExpr] -> [DvxExpr]
parseTokens TSemicolon lst = DvxList [] : lst
parseTokens TComma     lst = lst
parseTokens TSpace     lst = DvxList lst : []
parseTokens x          []  = DvxList [DvxTok x] : []
parseTokens x          lst = prependList (head lst) x : tail lst

prependList :: DvxExpr -> DvxToken -> DvxExpr
prependList (DvxTok   x) y = DvxList [DvxTok x, DvxTok y]
prependList (DvxList  x) y = DvxList $ DvxTok y : x

-- |Given a String, returns the corresponding token with its semantic value, if any.
parseValue :: String -> DvxToken
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

tokenize :: [String] -> [DvxToken]
tokenize =
    map parseValue . nonempty . tokenizeLine . joinsub . map stripComments
    where
    stripComments = takeWhile (/= 'U')
    tokenizeLine  = splitAndKeep separators . trim
    nonempty      = filter (not . null)
