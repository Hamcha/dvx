module Std
( stdContext
) where

import Dvx.Interpreter (Context)
import Dvx.Parser

stdContext :: Context
stdContext = [("SCRIVO", TypeFun stdPrint)
             ,("ACCAPO", TypeStr "\n")
             ,("PIV",    TypeFun $ stdArith (+) 0)
             ,("MENO",   TypeFun $ stdArith (-) 0)
             ,("PER",    TypeFun $ stdArith (*) 1)
             ,("DIVISO", TypeFun $ stdArith div 1)
             ]

stdPrint :: Function
stdPrint [] = return TypeNil
stdPrint ((TypeStr s):r) = do putStr s;        stdPrint r
stdPrint ((TypeInt i):r) = do putStr $ show i; stdPrint r
stdPrint ((TypeNil  ):r) = do putStr "null";   stdPrint r
stdPrint (x          :r) = do print x;         stdPrint r

stdArith :: Monad m => (Int -> Int -> Int) -> Int -> [DvxValue] -> m DvxValue
stdArith f neuter = return . TypeInt . func neuter
             where
             func :: Int -> [DvxValue] -> Int
             func n [] = n
             func n (TypeInt x:xs) = f x (func n xs)
             func _ _ = error "stdArith: invalid operands"
