module Dvx.Tokens
( token
, DvxToken(..)
) where

data DvxToken = TBool      Bool
              | TColon
              | TComma
              | TDefn
              | TDefnArgs
              | TDefVar
              | TNullCall
              | TPeriod
              | TName      String
              | TNil
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
token "VERO"      = TBool True
token "FALSO"     = TBool False
token "NIENTE"    = TNil
token x           = TName x
