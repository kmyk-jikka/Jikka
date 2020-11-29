{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Converter.Core.Simplify
-- Description : remove redundant things from AST.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.Simplify` remove redundant things from AST.
-- This step only removes bad patterns which are blamed by normal linters like HLint.
-- This step does nothing that affects optimization. Also this doesn't fold constants.
--
-- For example, in this step, @not not p@ is reduced to just @p@.
-- However, in this step, @k * (a + b)@ is not reduced to @k * a + k * b@, and vice versa.
module Jikka.Converter.Core.Simplify
  ( run,
  )
where

import Debug.Trace
import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr
import Jikka.Language.Core.Lint (typecheckProgram')

type TypeEnv = [(VarName, (Type, Maybe Expr))]

insertFormalArgs :: [(VarName, Type)] -> TypeEnv -> TypeEnv
insertFormalArgs [] env = env
insertFormalArgs ((x, t) : args) env = insertFormalArgs args ((x, (t, Nothing)) : env)

simplifyApp :: TypeEnv -> Expr -> [Expr] -> Expr
simplifyApp env f args = case f of
  Lit (LitBuiltin builtin) -> case (builtin, args) of
    -- arithmetical functions
    (Negate, [AppBuiltin Negate [e]]) -> e
    (Plus, [e1, e2]) | e1 == Lit0 -> e2
    (Plus, [e1, e2]) | e2 == Lit0 -> e1
    (Minus, [e1, e2]) | e1 == Lit0 -> AppBuiltin Negate [e2]
    (Minus, [e1, e2]) | e2 == Lit0 -> e1
    (Mult, [e1, _]) | e1 == Lit0 -> Lit0
    (Mult, [_, e2]) | e2 == Lit0 -> Lit0
    (Mult, [e1, e2]) | e1 == Lit1 -> e2
    (Mult, [e1, e2]) | e2 == Lit1 -> e1
    (FloorDiv, [e1, e2]) | e1 == Lit0 -> e2
    (FloorDiv, [e1, e2]) | e2 == Lit1 -> e1
    (FloorMod, [e1, _]) | e1 == Lit0 -> Lit0
    (FloorMod, [_, e2]) | e2 == Lit1 -> Lit0
    (Pow, [e1, _]) | e1 == Lit1 -> Lit1
    (Pow, [_, e2]) | e2 == Lit0 -> Lit1
    (Pow, [e1, e2]) | e2 == Lit1 -> e1
    -- advanced arithmetical functions
    (Abs, [AppBuiltin Abs [e]]) -> AppBuiltin Abs [e]
    (Gcd, [e1, e2]) | e1 == Lit0 -> e2
    (Gcd, [e1, e2]) | e2 == Lit0 -> e1
    (Gcd, [e1, _]) | e1 == Lit1 -> Lit1
    (Gcd, [_, e2]) | e2 == Lit1 -> Lit1
    (Gcd, [e1, e2]) | e1 == e2 -> e1
    (Lcm, [e1, _]) | e1 == Lit0 -> Lit0
    (Lcm, [_, e2]) | e2 == Lit0 -> Lit0
    (Lcm, [e1, e2]) | e1 == Lit1 -> e2
    (Lcm, [e1, e2]) | e2 == Lit1 -> e1
    (Lcm, [e1, e2]) | e1 == e2 -> e1
    (Min, [e1, e2]) | e1 == e2 -> e1
    (Max, [e1, e2]) | e1 == e2 -> e1
    -- logical functions
    (Not, [AppBuiltin Not [e]]) -> e
    (And, [e1, e2]) | e1 == LitTrue -> e2
    (And, [e1, e2]) | e2 == LitTrue -> e1
    (And, [e1, _]) | e1 == LitFalse -> LitFalse
    (And, [_, e2]) | e2 == LitFalse -> LitFalse
    (Or, [e1, _]) | e1 == LitTrue -> LitTrue
    (Or, [_, e2]) | e2 == LitTrue -> LitTrue
    (Or, [e1, e2]) | e1 == LitFalse -> e2
    (Or, [e1, e2]) | e2 == LitFalse -> e1
    (Implies, [e1, e2]) | e1 == LitTrue -> e2
    (Implies, [_, e2]) | e2 == LitTrue -> LitTrue
    (Implies, [e1, _]) | e1 == LitFalse -> LitTrue
    (Implies, [e1, e2]) | e2 == LitFalse -> simplifyExpr env (AppBuiltin Not [e1])
    (If t, [e1, e2, _]) | e1 == LitTrue -> e2
    (If t, [e1, _, e3]) | e1 == LitFalse -> e3
    (If t, [_, e2, e3]) | e2 == e3 -> e2
    -- bitwise functions
    (BitNot, [AppBuiltin BitNot [e]]) -> e
    (BitAnd, [e1, _]) | e1 == Lit0 -> Lit0
    (BitAnd, [_, e2]) | e2 == Lit0 -> Lit0
    (BitOr, [e1, e2]) | e1 == Lit0 -> e2
    (BitOr, [e1, e2]) | e2 == Lit0 -> e1
    (BitXor, [e1, e2]) | e1 == Lit0 -> e2
    (BitXor, [e1, e2]) | e2 == Lit0 -> e1
    (BitLeftShift, [e1, e2]) | e2 == Lit0 -> e1
    (BitRightShift, [e1, e2]) | e2 == Lit0 -> e1
    -- modular functions
    (Inv, [AppBuiltin Inv [n, m2], m1]) | m1 == m2 -> n -- We can assume they have values.
        -- list functions
    (Len _, [AppBuiltin (Tabulate _) [e, _]]) -> e
    (Len _, [AppBuiltin Range1 [e]]) -> e
    (Len _, [AppBuiltin Range2 [e1, e2]]) -> AppBuiltin Max [Lit0, AppBuiltin Minus [e2, e1]]
    (Tabulate _, [n, LamId _ _]) -> simplifyExpr env (AppBuiltin Range1 [n])
    (Sorted _, [AppBuiltin (Sorted t) [e]]) -> AppBuiltin (Sorted t) [e]
    (List _, [e]) -> e
    (Reversed _, [AppBuiltin (Reversed _) [e]]) -> e
    (Range2, [e1, e2]) | e1 == Lit0 -> simplifyExpr env (AppBuiltin Range1 [e2])
    (Range3, [e1, e2, e3]) | e3 == Lit1 -> simplifyExpr env (AppBuiltin Range2 [e1, e2])
    -- arithmetical relations
    -- equality relations (polymorphic)
    -- combinational functions
    _ -> App f args
  _ -> App f args

isVarUsed :: VarName -> Expr -> Bool
isVarUsed x = \case
  Var y -> y == x
  Lit _ -> False
  App f args -> isVarUsed x f || any (isVarUsed x) args
  Lam args e -> x `notElem` map fst args && isVarUsed x e
  Let y _ e1 e2 -> (y /= x && isVarUsed x e1) || isVarUsed x e2

simplifyLet :: TypeEnv -> VarName -> Type -> Expr -> Expr -> Expr
simplifyLet env x t e1 e2
  | not (isVarUsed x e2) = e2
  | otherwise = Let x t e1 e2

simplifyExpr :: TypeEnv -> Expr -> Expr
simplifyExpr env = \case
  Var x -> Var x
  Lit lit -> Lit lit
  App f args ->
    let args' = map (simplifyExpr env) args
     in simplifyApp env f args'
  Lam args e -> Lam args (simplifyExpr (insertFormalArgs args env) e)
  Let x t e1 e2 ->
    let e1' = simplifyExpr env e1
        e2' = simplifyExpr ((x, (t, Just e1')) : env) e2
     in simplifyLet env x t e1' e2'

simplifyToplevelExpr :: TypeEnv -> ToplevelExpr -> ToplevelExpr
simplifyToplevelExpr env = \case
  ResultExpr e -> ResultExpr $ simplifyExpr env e
  ToplevelLet Rec x args ret body cont
    | not (isVarUsed x body) ->
      simplifyToplevelExpr env (ToplevelLet NonRec x args ret body cont)
  ToplevelLet rec x args ret body cont ->
    let t = FunTy (map snd args) ret
        body' = case rec of
          NonRec -> simplifyExpr (insertFormalArgs args env) body
          Rec -> simplifyExpr (insertFormalArgs args $ (x, (t, Nothing)) : env) body'
        cont' = case rec of
          NonRec -> simplifyToplevelExpr ((x, (t, Just body)) : env) cont
          Rec -> simplifyToplevelExpr ((x, (t, Nothing)) : env) cont
     in ToplevelLet rec x args ret body' cont'

run :: Program -> Either String Program
run = typecheckProgram' . simplifyToplevelExpr []
