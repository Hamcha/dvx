module Dvx.Parser
( tokenize
--, parseLine
) where

import Dvx.Utils (trim, splitAndKeep)
import Dvx.Romans

data DvxExpr = DvxToken String | DvxList [DvxExpr] deriving Show

--parseLine :: [String] -> DvxExpr
--parseLine []                  = DvxList  []
--parseLine (x:y:xs) | y == " " = DvxList  (DvxToken x : [parseLine xs])

tokenize :: [String] -> [[String]]
tokenize =
    nonempty . map (romanize . nonempty . tokenizeLine)
    where
    tokenizeLine = splitAndKeep " ,.!:" [] . trim
    nonempty = filter (not . null)
