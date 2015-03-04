{-|
Module      : Dvx.Interpreter
Description : DVX Interpreter functions
-}
module Dvx.Interpreter
( execute
, getVar
) where

import Dvx.Parser

-- |Executes a list of expressions
execute :: [Context] -- ^ Current context chain (initial context when first called)
        -> [DvxExpr] -- ^ List of expressions to evaluate
        -> IO ()
execute _ []     = return ()
execute c (x:xs) = executeExpr c x >>= \cx -> execute (snd cx) xs

-- |Executes a single expression, returning a value and updated context
-- |Throws an error if the expression can't be evaluated
executeExpr :: [Context] -- ^ Current context chain
            -> DvxExpr   -- ^ Expression to evaluate
            -> IO (DvxValue, [Context])
executeExpr c DvxStart                = return (TypeNil, c)
executeExpr c (DvxDecl name value)    = setVar c name value
executeExpr c (DvxCall fn args)       = resolve c args
                                        >>= \var -> apply c (getVar c fn) var
                                        >>= \x -> return (x, c)
executeExpr c (DvxFunc name arg body) = return (f, appendContext c name f)
                                        where f = TypeFun $ makeFunction arg body
executeExpr c (DvxIf cond yes no)     = executeExpr c cond
                                        >>= \v-> case fst v of
                                                    TypeBool True  -> executeExpr c yes
                                                    TypeBool False -> executeExpr c no
                                                    _              -> error "TODO - toConditional"
executeExpr _ x                       = error $ "Can't execute expression: " ++ show x

-- |Adds a value (variable, function def) to the head of a context chain
appendContext :: [Context] -- ^ Context to append value to
              -> String    -- ^ Name of the value to append
              -> DvxValue  -- ^ Value to append
              -> [Context] -- ^ Resulting context
appendContext c str value = ((str, value) : head c) : tail c

-- |Gets a variable from the context chain
-- |Throws an error if it can't find the variable
getVar :: [Context] -- ^ Context chain to search variable from
       -> String    -- ^ Name to search for
       -> DvxValue  -- ^ Resulting value
getVar []     str = error $ "Undefined value: " ++ str
getVar (c:cs) str =
    case lookup str c of
        Just value -> value
        Nothing    -> getVar cs str

-- |Sets a variable in the context chain
setVar :: [Context] -- ^ Context chain to add variable to
       -> String    -- ^ Name of the variable to add
       -> DvxExpr   -- ^ Expresion of the variable to add (resolved in place)
       -> IO (DvxValue, [Context])
setVar c str expr = resolveValue c expr
                    >>= \value -> return (value, appendContext c str value)

-- |Creates a function from a list of arguments and a return expression
makeFunction :: [String] -- ^ Argument list
             -> DvxExpr  -- ^ Expression body
             -> Function -- ^ Resulting function
makeFunction args body c fargs = return . fst =<< executeExpr (ctx:c) body
                                 where ctx = bindArgs args fargs

-- |Creates a function context based from a list of arguments and their values
bindArgs :: [String]    -- ^ Argument list
         -> [DvxValue]  -- ^ Value list
         -> Context     -- ^ Resulting context
bindArgs []     []     = []
bindArgs []     _      = error "Argument count mismatch (too many)"
bindArgs _      []     = error "Argument count mismatch (too few)"
bindArgs (a:ax) (v:vx) = (a, v) : bindArgs ax vx

-- |Executes an expression where possible, otherwise returns nil
apply :: [Context]   -- ^ Context chain
      -> DvxValue    -- ^ Function resolved from the expression
      -> [DvxValue]  -- ^ List of arguments applied to the function
      -> IO DvxValue -- ^ Function's return value
apply c (TypeFun f) args = f c args
apply _ _           _    = return TypeNil

-- |Retrieves a list of values from a list of expressions
-- |This is just a wrapper over resolveValue
resolve :: [Context]     -- ^ Context chain to resolve from
        -> [DvxExpr]     -- ^ Expressions to resolve
        -> IO [DvxValue] -- ^ Resolved values
resolve _ []     = return []
resolve c (x:xs) = resolveValue c x
                   >>= \val -> resolve c xs
                   >>= \list -> return $ val:list

-- |Retrieves a value from an expression
-- |Works by executing the expression where possible or just looking around in
-- |the context chain.
resolveValue :: [Context]   -- ^ Context chain to resolve from
             -> DvxExpr     -- ^ Expression to resolve
             -> IO DvxValue -- ^ Resolved value
resolveValue _ (DvxConst x)      = return x
resolveValue c (DvxVar s)        = return $ getVar c s
resolveValue c (DvxCall fn args) = return . fst =<< executeExpr c (DvxCall fn args)
resolveValue _ v                 = error $ "Can't resolve " ++ show v
