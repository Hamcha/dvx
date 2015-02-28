module Dvx.Tokens
( token
, DvxValue(..)
) where

data DvxValue = TColon
              | TComma
              | TDefn
              | TDefnArgs
              | TDefnBody
              | TDefVar
              | TPeriod
              | TName      String
              | TNumber    Int
              | TPrelude
              | TSemicolon
              | TSpace
              | TString    String
              | TVarValue
              deriving (Eq, Show)

-- keywords definition
token :: String -> DvxValue
token " "             = TSpace
token ","             = TComma
token "!"             = TPeriod
token "."             = TPeriod
token ":"             = TColon
token ";"             = TSemicolon
token "ITALIANI"      = TPrelude
token "DEFINENDO"     = TDefn
token "OVE"           = TDefnArgs
token "È"             = TDefnBody
token "NOMINO"        = TDefVar
token "COME"          = TVarValue
token x               = TName x
