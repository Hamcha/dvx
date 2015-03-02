module Dvx.Interpreter
( Context
, execute
) where

import Dvx.Parser

type Context = [(String, DvxValue)]

execute :: [Context] -> [DvxExpr] -> IO ()
execute _ []     = return ()
execute c (x:xs) = executeExpr c x >>= \cx -> execute (snd cx) xs

executeExpr :: [Context] -> DvxExpr ->IO (DvxValue, [Context])
executeExpr c (DvxCall fn args) = apply (getVar c fn) args >>= \x -> return (x, c)
executeExpr c _                 = return (TypeNil, c)

getVar :: [Context] -> String -> DvxValue
getVar []     str = error $ "Undefined value: " ++ str
getVar (c:cs) str =
    case lookup str c of
        Just value -> value
        Nothing    -> getVar cs str

apply :: DvxValue -> [DvxExpr] -> IO DvxValue
apply (TypeFun f) args = f args
apply _           _    = return TypeNil