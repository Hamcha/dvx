module Dvx.Tokens
( token
, DvxValue(..)
) where

data DvxValue = TColon
              | TComma
              | TDefn
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

-- |Keywords definition
token :: String -> DvxValue
token " "             = TSpace
token ","             = TComma
token "!"             = TPeriod
token "."             = TPeriod
token ":"             = TColon
token ";"             = TSemicolon
token "ITALIANI"      = TPrelude
token "DEFINENDO"     = TDefn
token "NOMINO"        = TDefVar
token "COME"          = TVarValue
token "DI"            = TSpace
token "\9500\234"     = TSpace     -- È
token x               = TName x
