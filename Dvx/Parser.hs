module Dvx.Parser
( tokenize
) where

import Dvx.Utils (trim, splitAndKeep)

tokenize :: [String] -> [[String]]
tokenize =
    nonempty . map (nonempty . tokenizeLine)
    where
    tokenizeLine = splitAndKeep " ,.!:" [] . trim
    nonempty = filter (not . null)
