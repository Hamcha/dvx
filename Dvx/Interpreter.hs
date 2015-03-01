module Dvx.Interpreter
( execute
) where

import Dvx.Parser

type Context = [(String, DvxValue)]

execute :: [Context] -> [DvxExpr] -> IO ()
execute _ []     = return ()
execute c (x:xs) = executeExpr c x >>= \cx -> execute cx xs

executeExpr :: [Context] -> DvxExpr ->IO [Context]
executeExpr c _ = return c