module Dvx.Interpreter
( Context
, execute
) where

import Dvx.Parser

type Context = [(String, DvxValue)]

execute :: [Context] -> [DvxExpr] -> IO ()
execute _ []     = return ()
execute c (x:xs) = executeExpr c x >>= \cx -> execute (snd cx) xs

executeExpr :: [Context] -> DvxExpr -> IO (DvxValue, [Context])
executeExpr c DvxStart          = return (TypeNil, c)
executeExpr c (DvxDecl name v)  = setVar c name v
executeExpr c (DvxCall fn args) = resolve c args
                                  >>= \var -> apply (getVar c fn) var
                                  >>= \x -> return (x, c)
executeExpr _ x                 = error $ "Can't execute expression: " ++ show x

getVar :: [Context] -> String -> DvxValue
getVar []     str = error $ "Undefined value: " ++ str
getVar (c:cs) str =
    case lookup str c of
        Just value -> value
        Nothing    -> getVar cs str

setVar :: [Context] -> String -> DvxExpr -> IO (DvxValue, [Context])
setVar c str expr = resolveValue c expr
                    >>= \value -> return (value, ((str, value) : head c) : tail c)

apply :: DvxValue -> [DvxValue] -> IO DvxValue
apply (TypeFun f) args = f args
apply _           _    = return TypeNil

resolve :: [Context] -> [DvxExpr] -> IO [DvxValue]
resolve _ []     = return []
resolve c (x:xs) = resolveValue c x
                   >>= \val -> resolve c xs
                   >>= \list -> return $ val:list

resolveValue :: [Context] -> DvxExpr -> IO DvxValue
resolveValue _ (DvxConst x)      = return x
resolveValue c (DvxVar s)        = return $ getVar c s
resolveValue c (DvxCall fn args) = return . fst =<< executeExpr c (DvxCall fn args)
resolveValue _ v                 = error $ "Can't resolve " ++ show v
