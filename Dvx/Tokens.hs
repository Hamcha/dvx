module Dvx.Tokens
( token
, DvxTokName(..)
) where

import Dvx.Utils (isNumeric)

data DvxTokName = BANG
		| COLON
                | COMMA
		| DEFN
		| DEFNARGS
		| DEFNBODY
		| DEFVAR
		| DOT
		| FNCALLBODY
		| NAME
		| NUMBER
		| PRELUDE
		| SPACE
		| STRING
		| VARVALUE
		deriving (Eq, Show)

-- keywords definition
token :: String -> DvxTokName
token " "             = SPACE
token ","             = COMMA
token "!"             = BANG
token "."             = DOT
token ":"             = COLON
token "ITALIANI"      = PRELUDE
token "DEFINENDO"     = DEFN
token "OVE"           = DEFNARGS
token "È"             = DEFNBODY
token "NOMINO"        = DEFVAR
token "COME"          = VARVALUE
token "DI"            = FNCALLBODY
token x               = NAME
