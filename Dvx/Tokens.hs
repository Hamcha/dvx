{-|
Module      : Dvx.Tokens
Description : DVX tokens
-}
module Dvx.Tokens
( token
, DvxToken(..)
) where

-- |DVX Tokens (derived from code, used in AST building)
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
              | TIf
              | TThen
              | TElse
              deriving (Eq, Show)

-- |Keywords definition
token :: String -> DvxToken
token " "          = TSpace
token ","          = TComma
token "!"          = TPeriod
token "."          = TPeriod
token ":"          = TColon
token ";"          = TSemicolon
token "ITALIANI"   = TPrelude
token "DEFINENDO"  = TDefn
token "NOMINO"     = TDefVar
token "COME"       = TVarValue
token "DI"         = TNullCall
token "\9500\234"  = TNullCall  -- È
token "È"          = TNullCall  -- È
token "OVE"        = TDefnArgs
token "VERO"       = TBool True
token "FALSO"      = TBool False
token "NIENTE"     = TNil
token "SE"         = TIf
token "ALLORA"     = TThen
token "SENNÒ"      = TElse
token "ALTRIMENTI" = TElse
token x            = TName x
