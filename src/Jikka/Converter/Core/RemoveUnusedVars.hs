{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Converter.Core.RemoveUnusedVars
-- Description : remove unused variables from exprs.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.RemoveUnusedVars` remove unused variables from exprs.
module Jikka.Converter.Core.RemoveUnusedVars
  ( run,
  )
where

import Jikka.Language.Common.Name
import Jikka.Language.Core.Expr
import Jikka.Language.Core.Lint (typecheckProgram')
import Jikka.Language.Core.Vars (isUnusedVar)

cleanLet :: VarName -> Type -> Expr -> Expr -> Expr
cleanLet x t e1 e2
  | isUnusedVar x e2 = e2
  | otherwise = Let x t e1 e2

cleanExpr :: Expr -> Expr
cleanExpr = \case
  Var x -> Var x
  Lit lit -> Lit lit
  App f args -> App (cleanExpr f) (map cleanExpr args)
  Lam args e -> Lam args (cleanExpr e)
  Let x t e1 e2 -> cleanLet x t (cleanExpr e1) (cleanExpr e2)

cleanToplevelExpr :: ToplevelExpr -> ToplevelExpr
cleanToplevelExpr e = case e of
  ResultExpr e -> ResultExpr $ cleanExpr e
  ToplevelLet Rec x args ret body cont
    | isUnusedVar x body ->
      cleanToplevelExpr (ToplevelLet NonRec x args ret body cont)
  ToplevelLet rec x args ret body cont ->
    let body' = case rec of
          NonRec -> cleanExpr body
          Rec -> cleanExpr body
        cont' = case rec of
          NonRec -> cleanToplevelExpr cont
          Rec -> cleanToplevelExpr cont
     in ToplevelLet rec x args ret body' cont'

run :: Program -> Either String Program
run = typecheckProgram' . cleanToplevelExpr
