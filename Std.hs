module Std
( stdContext
) where

import Dvx.Interpreter (Context)
import Dvx.Parser

stdContext :: Context
stdContext = [("SCRIVO", TypeFun stdPrint)]

stdPrint :: Function
stdPrint (x:_) = do
    print x
    return TypeNil