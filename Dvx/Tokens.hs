module Dvx.Tokens
( token
, DvxToken(..)
) where

data DvxToken = TColon
              | TComma
              | TDefn
              | TDefnArgs
              | TDefVar
              | TNullCall
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
token :: String -> DvxToken
token " "         = TSpace
token ","         = TComma
token "!"         = TPeriod
token "."         = TPeriod
token ":"         = TColon
token ";"         = TSemicolon
token "ITALIANI"  = TPrelude
token "DEFINENDO" = TDefn
token "NOMINO"    = TDefVar
token "COME"      = TVarValue
token "DI"        = TNullCall
token "\9500\234" = TNullCall  -- È
token "È"         = TNullCall  -- È
token "OVE"       = TDefnArgs
token x           = TName x
