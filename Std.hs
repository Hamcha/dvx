-- |Dvx Standard Context
module Std
( stdContext
) where

import Dvx.Interpreter (setVar)
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
stdContext = [("DICO"    , TypeFun $ noContext stdPrint)
             ,("ANNVNCIO", TypeFun $ noContext stdPrint)
             ,("DIMMI"   , TypeFun stdRead)
             ,("ACCAPO"  , TypeStr "\n")
             ,("PIV"     , TypeFun $ noContext $ stdArith (+) 0)
             ,("MENO"    , TypeFun $ noContext $ stdArith (-) 0)
             ,("PER"     , TypeFun $ noContext $ stdArith (*) 1)
             ,("DIVISO"  , TypeFun $ noContext $ stdArith div 1)
             ,("MAGGIORE", TypeFun $ noContext $ stdCmpOrd (>))
             ,("MINORE"  , TypeFun $ noContext $ stdCmpOrd (<))
             ,("VGVALE"  , TypeFun $ noContext $ stdCmpEq (==))
             ,("DIVERSO" , TypeFun $ noContext $ stdCmpEq (/=))
             ,("QVALCVNO", TypeFun $ noContext $ stdLogic (||) False)
             ,("TVTTI"   , TypeFun $ noContext $ stdLogic (&&) True)
             ]


noContext :: ([DvxValue] -> IO DvxValue) -> Function
noContext f c args = f args >>= \ret -> return (ret, c)

stdPrint :: [DvxValue] -> IO DvxValue
stdPrint [] = return TypeNil
stdPrint ((TypeStr s) :r) = do putStr s;        stdPrint r
stdPrint ((TypeInt i) :r) = do putStr $ show i; stdPrint r
stdPrint ((TypeBool b):r) = do print b;         stdPrint r
stdPrint ((TypeNil  ) :r) = do putStr "null";   stdPrint r
stdPrint (x           :r) = do print x;         stdPrint r

stdRead :: Function
stdRead c [] = return (TypeNil, c)
stdRead c ((TypeStr x):xs) = do val <- getLine
                                res <- setVar c x $ DvxConst $ TypeStr val
                                stdRead (snd res) xs
stdRead _ _ = error "stdRead: invalid operands"


stdArith :: (Int -> Int -> Int) -> Int -> [DvxValue] -> IO DvxValue
stdArith f neuter = return . TypeInt . func neuter
                    where
                    func :: Int -> [DvxValue] -> Int
                    func n [] = n
                    func n (TypeInt x:xs) = f x (func n xs)
                    func _ _ = error "stdArith: invalid operands"

stdCmpEq :: (DvxComparable a -> DvxComparable b-> Bool) -> [DvxValue] -> IO DvxValue
stdCmpEq f = return . TypeBool . func
             where
             func :: [DvxValue] -> Bool
             func (x:y:[]) = f (toComparable x) (toComparable y)
             func (x:xs) = f (toComparable x) (CompBool (func xs))
             func _ = error "stdCmpEq: invalid operands"

stdCmpOrd :: (DvxComparable a -> DvxComparable b -> Bool) -> [DvxValue] -> IO DvxValue
stdCmpOrd f = return . TypeBool . func
              where
              func :: [DvxValue] -> Bool
              func (x:y:[]) = f (toComparable x) (toComparable y)
              func _ = error "stdCmpOrd: invalid number of arguments"

stdLogic :: (Bool -> Bool -> Bool) -> Bool -> [DvxValue] -> IO DvxValue
stdLogic f dflt = return . TypeBool . func dflt
                  where
                  func :: Bool -> [DvxValue] -> Bool
                  func def [] = def
                  func def (TypeBool x:xs) = f x (func def xs)
                  func def (TypeInt  x:xs) = f (x /= 0) (func def xs)
                  func def (TypeStr  x:xs) = f (not $ null x) (func def xs)
                  func _ _ = error "stdLogic: invalid operands"
