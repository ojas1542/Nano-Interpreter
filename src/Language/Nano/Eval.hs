{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser
import Text.ParserCombinators.ReadP (look)
import GHC.Base (VecCount(Vec2))
import GHC.RTS.Flags (DoHeapProfile(HeapByClosureType))
import Data.Functor.Product (Product(Pair))

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

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
-- >>> parse "let sumList x s = if x == [] then s else sumList (tail x) (s + head x) in sumList [1,2,3,4] 0"
-- ELet "sumList" (ELam "x" (ELam "s" (EIf (EBin Eq (EVar "x") ENil) (EVar "s") (EApp (EApp (EVar "sumList") (EApp (EVar "tail") (EVar "x"))) (EBin Plus (EVar "s") (EApp (EVar "head") (EVar "x"))))))) (EApp (EApp (EVar "sumList") (EBin Cons (EInt 1) (EBin Cons (EInt 2) (EBin Cons (EInt 3) (EBin Cons (EInt 4) ENil))))) (EInt 0))
--
-- >>> parse "  let map f xs = if xs == [] then [] else let h = head xs in let t = tail xs in f h : map f t in let incr x = x + 1 in let l = [1, 2, 3, 4] in map incr l"
-- ELet "map" (ELam "f" (ELam "xs" (EIf (EBin Eq (EVar "xs") ENil) ENil (ELet "h" (EApp (EVar "head") (EVar "xs")) (ELet "t" (EApp (EVar "tail") (EVar "xs")) (EBin Cons (EApp (EVar "f") (EVar "h")) (EApp (EApp (EVar "map") (EVar "f")) (EVar "t")))))))) (ELet "incr" (ELam "x" (EBin Plus (EVar "x") (EInt 1))) (ELet "l" (EBin Cons (EInt 1) (EBin Cons (EInt 2) (EBin Cons (EInt 3) (EBin Cons (EInt 4) ENil)))) (EApp (EApp (EVar "map") (EVar "incr")) (EVar "l"))))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

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
-- Error {errMsg = "unbound variable: p"}
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
-- Error {errMsg = "type error"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- Error {errMsg = "type error"}
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
-- unknown command '{'
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
-- >>> execExpr el
-- (1 : (3 : (5 : [])))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (3 : (5 : []))
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval env (EInt i) = VInt i
eval env ENil = VNil
eval env (EVar "head") = VPrim f
  where
    f (VPair i _) = i
    f _ = throw (Error "type error")
eval env (EVar "tail") = VPrim f
  where
    f (VPair (VInt _) (VPair x y)) = VPair x y
    f (VPair (VInt _) VNil) = VNil
    f  _ = throw (Error "type error")
eval env (EVar id) = lookupId id env
eval env (EBool b) = VBool b
eval env (EBin op e1 e2) = helper v1 v2
  where
    v1 = eval env e1
    v2 = eval env e2

    helper VNil x = case op of
      Eq -> VBool (VNil == x) --empty list equality
      _ -> throw (Error "type error")
    helper x VNil = case op of
      Eq -> VBool (VNil == x) --empty list equality
      Cons -> VPair x VNil --base case list constructor
      _ -> throw (Error "type error")
    
    helper (VPair x y) (VPair a b) = VBool (VPair x y == VPair a b) --list equality
    helper (VInt i1) (VPair x y) = VPair (VInt i1) (VPair x y) --construct list

    helper (VInt i1) (VInt i2) = case op of
      Plus -> VInt ((+) i1 i2)
      Minus -> VInt ((-) i1 i2)
      Mul -> VInt ((*) i1 i2)
      Div -> VInt (i1 `div` i2)
      Eq -> VBool (i1 == i2)
      Ne -> VBool (i1 /= i2)
      Lt -> VBool (i1 < i2)
      Le -> VBool (i1 <= i2)
      _ -> throw (Error "type error")

    helper (VBool i1) (VBool i2) = case op of
      Eq -> VBool (i1 == i2)
      Ne -> VBool (i1 /= i2)
      And -> VBool (i1 && i2)
      Or -> VBool (i1 || i2)
      _ -> throw (Error "type error")
    
    helper _ _ = throw (Error "type error")
eval env (EIf e1 e2 e3) = helper res
  where
    res = eval env e1
    helper (VBool b) = if b then eval env e2 else eval env e3
    helper _ = throw (Error "type error")
eval env (ELet id e1 e2) = eval env' e2
  where
    v = eval env e1
    env' = (id,v):env
eval env (EApp e1 e2) =
  let arg = eval env e2 in
    case eval env e1 of
      (VClos env' id e) -> let bodyEnv = (id,arg):env' ++ env in eval bodyEnv e
      VPrim f -> f arg
      _ -> throw (Error "type error")
eval env (ELam id e1) = VClos env id e1

--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp = error "TBD:evalOp"

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
-- Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId id (p:ps) = if id == fst p then snd p else lookupId id ps
lookupId id [] = throw (Error ("unbound variable: " ++ id))

prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
