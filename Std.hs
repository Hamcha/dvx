module Std
( stdContext
) where

import Dvx.Interpreter (Context)
import Dvx.Parser

stdContext :: Context
stdContext = [("SCRIVO", TypeFun stdPrint)
             ,("ACCAPO", TypeStr "\n")
             ,("PIV",    TypeFun $ stdArith (+))
             ,("MENO",   TypeFun $ stdArith (-))
             ,("PER",    TypeFun $ stdArith (*))
             ,("DIVISO", TypeFun $ stdArith div)
             ]

stdPrint :: Function
stdPrint [] = return TypeNil
stdPrint ((TypeStr s):r) = do putStr s;        stdPrint r
stdPrint ((TypeInt i):r) = do putStr $ show i; stdPrint r
stdPrint ((TypeNil  ):r) = do putStr "null";   stdPrint r
stdPrint (x          :r) = do print x;         stdPrint r

stdArith :: Monad m => (Int -> Int -> Int) -> [DvxValue] -> m DvxValue
stdArith f = return . TypeInt . func
             where
             func :: [DvxValue] -> Int
             func [] = 0
             func (TypeInt x:xs) = f x (func xs)
             func _ = error "stdArith: invalid operands"
