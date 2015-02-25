module Dvx.Tokens
( token
, DvxValue(..)
) where

import Dvx.Utils (isNumeric)

data DvxValue = BANG
              | COLON
              | COMMA
              | DEFN
              | DEFNARGS
              | DEFNBODY
              | DEFVAR
              | DOT
              | FNCALLBODY
              | NAME       String
              | NUMBER     Int
              | PRELUDE
              | SPACE
              | STRING     String
              | VARVALUE
              deriving (Eq, Show)

-- keywords definition
token :: String -> DvxValue
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
token x               = NAME x
