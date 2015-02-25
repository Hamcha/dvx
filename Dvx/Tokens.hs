module Dvx.Tokens
( token
, DvxTokName(..)
) where

import Dvx.Utils (isNumeric)

data DvxTokName = BANG
                | COMMA
		| DEFN
		| DEFNARGS
		| DEFNBODY
		| DEFVAR
		| FNCALLBODY
		| NUMBER
		| PRELUDE
		| SPACE
		| STRING
		| VARVALUE
		deriving (Eq, Show)

token :: String -> DvxTokName
token " "             = SPACE
token ","             = COMMA
token "!"             = BANG
token "ITALIANI"      = PRELUDE
token "DEFINENDO"     = DEFN
token "OVE"           = DEFNARGS
token "È"             = DEFNBODY
token "NOMINO"        = DEFVAR
token "COME"          = VARVALUE
token "DI"            = FNCALLBODY
token x | isNumeric x = NUMBER
        | otherwise   = STRING
