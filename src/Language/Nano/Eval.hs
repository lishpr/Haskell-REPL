{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( defsFile 
  , execFile, execString, execEnvString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import           System.Directory (doesFileExist)
import           Control.Exception (throw, catch)
import           Language.Nano.Types
import qualified Language.Nano.Parser as Parser

--------------------------------------------------------------------------------
defsFile :: FilePath -> IO [(Id, Expr)]
--------------------------------------------------------------------------------
defsFile f = safeRead f >>= return . Parser.parseDefs

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (safeRead f >>= execString) `catch` exitError

safeRead :: FilePath -> IO String
safeRead f = do
  b <- doesFileExist f
  if b then readFile f 
       else throw (Error ("unknown file: " ++ f))

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parse s) `catch` exitError

--------------------------------------------------------------------------------
execEnvString :: Env -> String -> IO Value
--------------------------------------------------------------------------------
execEnvString env s = return (eval env (parse s)) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = Parser.parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- 0
--
-- >>> eval env0 "p"
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (2 : [])

--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval env e = case evalE env e of 
  Left exn  -> exn
  Right val -> val

--------------------------------------------------------------------------------
evalE :: Env -> Expr -> Either Value Value
--------------------------------------------------------------------------------
evalE env (EInt i)                = Right (VInt i)
evalE env (EBool b)               = Right (VBool b)

evalE env (EVar x)                = Right (case lookupId x env of
                                                v@(VClos clEnv y e) -> VClos (extend x v clEnv) y e 
                                                v -> v)

evalE env (EBin o (EThr e1) e2)   = Left (eval env e1)
evalE env (EBin o e1 (EThr e2))   = Left (eval env e2)
evalE env (EBin o e1 e2)          = Right (evalOp o (eval env e1) (eval env e2))

evalE env (EIf c t e)             = do cond <- evalE env c
                                       case cond of
                                            VBool True  -> evalE env t
                                            VBool False -> evalE env e
                                            _           -> throw (Error "type error: if")
       
evalE env (ELet x e1 e2)          = evalE (extend x (eval env e1) env) e2
evalE env (EApp e1 e2)            = Right (evalApp (eval env e1) (eval env e2))
evalE env (ELam x e)              = Right (VClos env x e)
evalE env ENil                    = Right (VNil)

evalE env (EThr e)                = Left (eval env e)
                              
evalE env (ETry e1 x e2)          = case evalE env e1 of
                                         Left exn -> Left (eval (extend x (eval env e1) env) e2)
                                         Right val -> Right val

-- --------------------------------------------------------------------------------
-- Old eval from HW4:
-- eval :: Env -> Expr -> Value
-- --------------------------------------------------------------------------------
-- eval _   (EInt i)       = VInt i
-- eval _   (EBool b)      = VBool b
-- eval env (EVar x)       = case lookupId x env of
                            -- v@(VClos clEnv y e) -> VClos (extend x v clEnv) y e 
                            -- v -> v
-- eval env (EBin o e1 e2) = evalOp o (eval env e1) (eval env e2)
-- eval env (EIf c t e)    = case eval env c of
                            -- VBool True  -> eval env t
                            -- VBool False -> eval env e
                            -- _           -> throw (Error "type error: if")
-- eval env (ELet x e1 e2) = eval env1 e2
  -- where
    -- v1                  = eval env e1
    -- env1                = extend x v1 env
-- eval env (EApp e1 e2)   = evalApp (eval env e1) (eval env e2)
-- eval env (ELam x e)     = VClos env x e
-- eval _   ENil           = VNil 

--------------------------------------------------------------------------------
-- | Unit tests for `throw`
--------------------------------------------------------------------------------

-- 1 + 2                 ==> 3
-- (throw 1) + 2         ==> 1
-- 1 + (throw 2)         ==> 2
-- (throw 1) + (throw 2) ==> 1
-- throw (1 + 2)         ==> 3
-- throw (1 + (throw 2)) ==> 2

-- >>> let ex_1_2   = EBin Plus (EInt 1) (EInt 2)
-- >>> let ex_t1_2  = EBin Plus (EThr (EInt 1)) (EInt 2)
-- >>> let ex_1_t2  = EBin Plus (EInt 1) (EThr (EInt 2)) 
-- >>> let ex_t1_t2 = EBin Plus (EThr (EInt 1)) (EThr (EInt 2)) 
-- >>> let ex_t12   = EThr (EBin Plus (EInt 1) (EInt 2))
-- >>> let ex_tt12  = EThr (EBin Plus (EInt 1) (EThr (EInt 2)))

-- >>> eval [] ex_1_2 
-- 3
-- >>> eval [] ex_t1_2 
-- 1
-- >>> eval [] ex_1_t2 
-- 2
-- >>> eval [] ex_t1_t2 
-- 1
-- >>> eval [] ex_t12 
-- 3
-- >>> eval [] ex_tt12 
-- 2

--------------------------------------------------------------------------------
-- | Unit tests for `try-catch`
--------------------------------------------------------------------------------
-- try (1 + 2)         handle z => z + 10   ==> 3 
-- try ((throw 1) + 2) handle z => z + 10   ==> 11 
-- try (1 + (throw 2)) handle z => z + 10   ==> 12 
-- try ((throw 1) + (throw 2)) handle z => z + 10   ==> 11 

-- >>> eval [] (ETry ex_1_2 "z" (EBin Plus "z" (EInt 10))
-- 3
-- >>> eval [] (ETry ex_t1_2 "z" (EBin Plus "z" (EInt 10))
-- 11
-- >>> eval [] (ETry ex_1_t2 "z" (EBin Plus "z" (EInt 10))
-- 12
-- >>> eval [] (ETry ex_t1_t2 "z" (EBin Plus "z" (EInt 10))
-- 11
-- >>> eval [] (ETry ex_t12 "z" (EBin Plus "z" (EInt 10))
-- 13
-- >>> eval [] (ETry ex_tt12 "z" (EBin Plus "z" (EInt 10))
-- 12

--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus   (VInt n)     (VInt m)     = VInt  (n + m)
evalOp Minus  (VInt n)     (VInt m)     = VInt  (n - m)
evalOp Mul    (VInt n)     (VInt m)     = VInt  (n * m)
evalOp Div    (VInt n)     (VInt m)     = VInt  (n `div` m)
evalOp Eq     (VInt a)     (VInt b)     = VBool (a == b)
evalOp Eq     (VBool a)    (VBool b)    = VBool (a == b)
evalOp Eq     VNil         VNil         = VBool True
evalOp Eq     (VPair x xs) (VPair y ys) = let 
                                            VBool r1 = evalOp Eq x y 
                                            VBool r2 = evalOp Eq xs ys 
                                          in VBool (r1 && r2)
evalOp Eq     VNil         (VPair _ _)  = VBool False
evalOp Eq     (VPair _ _)  VNil         = VBool False
evalOp Ne     v1           v2           = let 
                                            VBool res = evalOp Eq v1 v2 
                                          in VBool (not res)
evalOp Lt     (VInt n)     (VInt m)     = VBool (n <  m)
evalOp Le     (VInt n)     (VInt m)     = VBool (n <= m)
evalOp And    (VBool b)    (VBool c)    = VBool (b && c)
evalOp Or     (VBool b)    (VBool c)    = VBool (b || c)
evalOp Cons   v1           v2           = VPair v1 v2
evalOp _      _        _                = throw (Error "type error: binop")

--------------------------------------------------------------------------------
evalApp :: Value -> Value -> Value
--------------------------------------------------------------------------------
evalApp (VClos env x e) v = eval (extend x v env) e
evalApp (VPrim f) v       = f v
evalApp _               _ = throw (Error "type error: closure")

--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId x ((y, v) : env)
  | x == y    = v
  | otherwise = lookupId x env
lookupId x [] = throw (Error ("unbound variable: " ++ x))

--------------------------------------------------------------------------------
extend :: Id -> Value -> Env -> Env
--------------------------------------------------------------------------------
extend x v env = (x, v) : env

prelude :: Env
prelude =
  [ ("tail", VPrim primTail)
  , ("head", VPrim primHead)
  ]

primHead :: Value -> Value
primHead (VPair x _) = x
primHead _           = throw (Error "head on a non-list")

primTail :: Value -> Value
primTail (VPair _ xs) = xs
primTail _            = throw (Error "tail on a non-list")

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------