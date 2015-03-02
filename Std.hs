module Std
( stdContext
) where

import Dvx.Interpreter (Context)
import Dvx.Parser

stdContext :: Context
stdContext = [("SCRIVO", TypeFun stdPrint)]

stdPrint :: Function
stdPrint ((TypeStr s):_) = do putStrLn s;        return TypeNil
stdPrint ((TypeInt i):_) = do putStrLn $ show i; return TypeNil
stdPrint ((TypeNil  ):_) = do putStrLn "null";   return TypeNil
stdPrint (x          :_) = do print x;           return TypeNil