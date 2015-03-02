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
executeExpr c (DvxDecl name v)  = return $ setVar c name v
executeExpr c (DvxCall fn args) = apply (getVar c fn) (resolve c args) >>= \x -> return (x, c)
executeExpr _ x                 = error $ "Can't execute expression: " ++ show x

getVar :: [Context] -> String -> DvxValue
getVar []     str = error $ "Undefined value: " ++ str
getVar (c:cs) str =
    case lookup str c of
        Just value -> value
        Nothing    -> getVar cs str

setVar :: [Context] -> String -> DvxExpr -> (DvxValue, [Context])
setVar c str expr = (value, newcontext : tail c)
                    where
                    newcontext = (str, value) : head c
                    value = resolveValue c expr

apply :: DvxValue -> [DvxValue] -> IO DvxValue
apply (TypeFun f) args = f args
apply _           _    = return TypeNil

resolve :: [Context] -> [DvxExpr] -> [DvxValue]
resolve c = map $ resolveValue c

resolveValue :: [Context] -> DvxExpr -> DvxValue
resolveValue _ (DvxConst x) = x
resolveValue c (DvxVar s)   = getVar c s
resolveValue _ v            = error $ "Can't resolve " ++ show v