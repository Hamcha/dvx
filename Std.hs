-- |Dvx Standard Context
module Std
( stdContext
) where

import Dvx.Parser

data DvxComparable a = CompNil
                     | CompBool Bool
                     | CompInt  Int
                     | CompStr  String
                     deriving (Eq, Ord)

toComparable :: DvxValue -> DvxComparable a
toComparable (TypeInt  a) = CompInt  a
toComparable (TypeBool a) = CompBool a
toComparable (TypeStr  a) = CompStr  a
toComparable TypeNil      = CompNil
toComparable x            = error $ show x ++ " is not comparable."

stdContext :: Context
stdContext = [("SCRIVO"  , TypeFun stdPrint)
             ,("ACCAPO"  , TypeStr "\n")
             ,("PIV"     , TypeFun $ stdArith (+) 0)
             ,("MENO"    , TypeFun $ stdArith (-) 0)
             ,("PER"     , TypeFun $ stdArith (*) 1)
             ,("DIVISO"  , TypeFun $ stdArith div 1)
             ,("MAGGIORE", TypeFun $ stdCmpOrd (>))
             ,("MINORE"  , TypeFun $ stdCmpOrd (<))
             ,("VGVALE"  , TypeFun $ stdCmpEq (==))
             ,("DIVERSO" , TypeFun $ stdCmpEq (/=))
             ,("QVALCVNO", TypeFun $ stdLogic (||) False)
             ,("TVTTI"   , TypeFun $ stdLogic (&&) True)
             ]

stdPrint :: Function
stdPrint _ [] = return TypeNil
stdPrint _ ((TypeStr s) :r) = do putStr s;        stdPrint [] r
stdPrint _ ((TypeInt i) :r) = do putStr $ show i; stdPrint [] r
stdPrint _ ((TypeBool b):r) = do print b;         stdPrint [] r
stdPrint _ ((TypeNil  ) :r) = do putStr "null";   stdPrint [] r
stdPrint _ (x           :r) = do print x;         stdPrint [] r

stdArith :: (Int -> Int -> Int) -> Int -> Function
stdArith f neuter _ = return . TypeInt . func neuter
                      where
                      func :: Int -> [DvxValue] -> Int
                      func n [] = n
                      func n (TypeInt x:xs) = f x (func n xs)
                      func _ _ = error "stdArith: invalid operands"

stdCmpEq :: (DvxComparable a -> DvxComparable b-> Bool) -> Function
stdCmpEq f _ = return . TypeBool . func
               where
               func :: [DvxValue] -> Bool
               func (x:y:[]) = f (toComparable x) (toComparable y)
               func (x:xs) = f (toComparable x) (CompBool (func xs))
               func _ = error "stdCmpEq: invalid operands"

stdCmpOrd :: (DvxComparable a -> DvxComparable b -> Bool) -> Function
stdCmpOrd f _ = return . TypeBool . func
                where
                func :: [DvxValue] -> Bool
                func (x:y:[]) = f (toComparable x) (toComparable y)
                func _ = error "stdCmpOrd: invalid number of arguments"

stdLogic :: (Bool -> Bool -> Bool) -> Bool -> Function
stdLogic f dflt _ = return . TypeBool . func dflt
                    where
                    func :: Bool -> [DvxValue] -> Bool
                    func def [] = def
                    func def (TypeBool x:xs) = f x (func def xs)
                    func def (TypeInt  x:xs) = f (x /= 0) (func def xs)
                    func def (TypeStr  x:xs) = f (not $ null x) (func def xs)
                    func _ _ = error "stdLogic: invalid operands"
