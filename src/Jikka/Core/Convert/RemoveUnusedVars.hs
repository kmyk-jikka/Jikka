{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.RemoveUnusedVars
-- Description : remove unused variables from exprs.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.RemoveUnusedVars` remove unused variables from exprs.
module Jikka.Core.Convert.RemoveUnusedVars
  ( run,
  )
where

import Jikka.Common.Error
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint (typecheckProgram')
import Jikka.Core.Language.Vars (isUnusedVar)

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
cleanToplevelExpr = \case
  ResultExpr e -> ResultExpr $ cleanExpr e
  ToplevelLet rec x args ret body cont ->
    let rec' = case rec of
          Rec | isUnusedVar x body -> NonRec
          _ -> rec
        body' = cleanExpr body
        cont' = cleanToplevelExpr cont
     in ToplevelLet rec' x args ret body' cont'

run :: MonadError Error m => Program -> m Program
run = typecheckProgram' . cleanToplevelExpr
