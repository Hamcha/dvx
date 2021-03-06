{-|
Module      : Dvx.Parser
Description : AST parsing and tokenizing functions
-}
module Dvx.Parser
( Context
, DvxExpr(..)
, DvxValue(..)
, DvxResult
, Function
, tokenize
, parse
, preparse
,
) where

import Dvx.Tokens
import Dvx.Romans
import Dvx.Utils

type Context = [(String, DvxValue)]
type DvxResult = (DvxValue, [Context])
type Function = [Context] -> [DvxValue] -> IO DvxResult

data DvxValue = TypeNil
              | TypeBool Bool
              | TypeInt  Int
              | TypeStr  String
              | TypeFun  Function
              | TypeLst  [DvxValue]

instance Show DvxValue where
    show (TypeNil)    = "Nil"
    show (TypeBool x) = "Bool " ++ show x
    show (TypeInt x)  = "Int "  ++ show x
    show (TypeStr x)  = "Str "  ++ show x
    show (TypeLst x)  = "Lst "  ++ show x
    show (TypeFun _)  = "Function <Function>"

-- | AST expression
data DvxExpr  = DvxTok   DvxToken                  -- ^ Unparsed token
              | DvxStart                           -- ^ "ITALIANI"
              | DvxConst DvxValue                  -- ^ Constant
              | DvxVar   String                    -- ^ Variable
              | DvxCall  String [DvxExpr]          -- ^ Function call
              | DvxFunc  String [String] DvxExpr   -- ^ Function definition
              | DvxIf    DvxExpr DvxExpr DvxExpr   -- ^ If/then/else
              | DvxDecl  String DvxExpr            -- ^ Variable declaration
              | DvxList  [DvxExpr]                 -- ^ List
              deriving Show

separators :: String
separators = "\t ,.!:;"

parse :: [DvxToken] -> [DvxExpr]
parse = joinsub . map (makeast . foldr parseTokens []) . splitOn TPeriod

preparse :: [DvxToken] -> [DvxExpr]
preparse = joinsub . map (foldr parseTokens []) . splitOn TPeriod

-- |Makes a fully parsed AST off a raw tree
makeast :: [DvxExpr] -> [DvxExpr]
makeast [] = []
-- Conditional branching
makeast (DvxTok TIf
        :DvxList (iftok
                 :iflist
                 :DvxList (DvxTok TThen
                          :DvxList (iftruetok
                                   :iftruelist
                                   :DvxList (DvxTok TElse
                                            :DvxList iffalse
                                            :_)
                                   :_)
                          :_)
                 :_)
        :ys)
        = DvxIf (makeast [iftok, iflist] !! 0)
                (makeast [iftruetok, iftruelist] !! 0)
                (makeast iffalse !! 0)
          : makeast ys
-- Function definition
makeast (DvxTok TDefn
        :DvxList (DvxTok (TName name)
                 :DvxList (DvxTok TDefnArgs
                          :DvxList args
                          :DvxList expr
                          :_)
                 :_)
        :ys)
        = DvxFunc (trim name) (getArgs args) (makeast expr !! 0) : makeast ys
-- Variable declaration
makeast (DvxTok TDefVar
        :DvxList (DvxTok value
                 :DvxList (DvxTok TVarValue
                          :DvxList (DvxTok (TName name):_)
                          :_)
                 :_)
        :ys)
        = DvxDecl (trim name) (discover value) : makeast ys
-- Nullcall (È, DI etc.)
makeast (DvxTok TNullCall  :xs) = makeast xs
-- Function call (no args)
makeast (DvxTok (TName x):DvxList [DvxTok TIgnore]:ys) = DvxCall x [] : makeast ys
-- Function call
makeast (DvxTok (TName x):DvxList y:ys) = DvxCall x (makeast y) : makeast ys
-- List of list
makeast (DvxList x:xs) = makeast x ++ makeast xs
-- Plain token
makeast (DvxTok  x:xs) = (discover x) : makeast xs

-- |Parses plain tokens (DvxTok) into something meaningful
discover :: DvxToken -> DvxExpr
discover TPrelude    = DvxStart
discover (TNumber n) = DvxConst $ TypeInt  n
discover (TString s) = DvxConst $ TypeStr  s
discover (TBool   b) = DvxConst $ TypeBool b
discover (TNil)      = DvxConst $ TypeNil
discover (TName   n) = DvxVar n
discover x = DvxTok x

-- |Gets a list of strings from a function declaration argument DvxList
getArgs :: [DvxExpr] -> [String]
getArgs []                    = []
getArgs (DvxTok TIgnore:xs)   = [] : getArgs xs
getArgs (DvxTok (TName x):xs) = x : getArgs xs
getArgs _                     = error "Invalid token in function declaration arguments"

-- |Parses a list plain tokens (DvxTok) into a tree
parseTokens :: DvxToken -> [DvxExpr] -> [DvxExpr]
parseTokens TSemicolon lst = DvxList [] : lst
parseTokens TComma     lst = lst
parseTokens TSpace     lst = DvxList lst : []
parseTokens x          []  = DvxList [DvxTok x] : []
parseTokens x          lst = prependList (head lst) x : tail lst

-- |Adds token to the beginning of a list, if both arguments are token, create a list of both
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

-- |Creates a list of tokens off lines of code
tokenize :: [String] -> [DvxToken]
tokenize =
    map parseValue . joinstr [] 0 . nonempty . tokenizeLine . joinsub . map stripComments
    where
    stripComments = takeWhile (/= 'U')
    tokenizeLine  = splitAndKeep separators . trim
    nonempty      = filter (not . null)
