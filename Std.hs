module Std
( stdContext
) where

import Dvx.Interpreter (Context)
import Dvx.Parser

stdContext :: Context
stdContext = [("SCRIVO", TypeFun stdPrint)
             ,("ACCAPO", TypeStr "\n")
             ]

stdPrint :: Function
stdPrint [] = return TypeNil
stdPrint ((TypeStr s):r) = do putStr s;        stdPrint r
stdPrint ((TypeInt i):r) = do putStr $ show i; stdPrint r
stdPrint ((TypeNil  ):r) = do putStr "null";   stdPrint r
stdPrint (x          :r) = do print x;         stdPrint r