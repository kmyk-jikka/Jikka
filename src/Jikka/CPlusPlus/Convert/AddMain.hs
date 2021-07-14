{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.AddMain
-- Description : adds @main@ function. / @main@ 関数を追加します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.AddMain
  ( run,
  )
where

import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Set as S
import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Common.IOFormat as F

cinStatement :: Expr -> Statement
cinStatement e = ExprStatement (BinOp BitRightShift (Var "std::cin") e)

coutStatement :: Expr -> Statement
coutStatement e = ExprStatement (BinOp BitLeftShift (BinOp BitLeftShift (Var "std::cout") e) (Lit (LitChar '\n')))

forStatement :: VarName -> Expr -> [Statement] -> Statement
forStatement i n body = For TyInt32 i (Lit (LitInt32 0)) (BinOp LessThan (Var i) n) (AssignIncr (LeftVar i)) body

lookup' :: (MonadState (M.Map String VarName) m, MonadError Error m) => String -> m VarName
lookup' x = do
  y <- gets $ M.lookup x
  case y of
    Just y -> return y
    Nothing -> throwInternalError $ "undefined variable: " ++ x

runFormatExpr :: (MonadState (M.Map String VarName) m, MonadAlpha m, MonadError Error m) => F.FormatExpr -> m Expr
runFormatExpr = \case
  F.Var x -> Var <$> lookup' x
  F.Plus e k -> BinOp Add <$> runFormatExpr e <*> pure (Lit (LitInt32 k))
  F.At e i -> At <$> runFormatExpr e <*> (Var <$> lookup' i)
  F.Len e -> do
    e <- runFormatExpr e
    return $ Cast TyInt32 (Call (Method e "size") [])

runMainDeclare :: (MonadState (M.Map String VarName) m, MonadAlpha m, MonadError Error m) => F.IOFormat -> m [(S.Set VarName, Statement)]
runMainDeclare format = go M.empty (F.inputTree format)
  where
    go sizes = \case
      F.Exp e -> do
        (x, indices) <- F.unpackSubscriptedVar e
        y <- renameVarName LocalNameKind x
        modify' $ M.insert x y
        let lookupSize i = case M.lookup i sizes of
              Just e -> return e
              Nothing -> throwInternalError $ "undefined variable" ++ i
        sizes' <- mapM lookupSize indices
        let deps = S.fromList (concatMap freeVars sizes')
        let t = foldl (\t _ -> TyVector t) TyInt64 indices
        let decl = Declare t y (Just (snd (foldr (\size (t, e) -> (TyVector t, Call (Function "std::vector" [t]) [size, e])) (TyInt64, Lit (LitInt64 (-1))) sizes')))
        return [(deps, decl)]
      F.Newline -> return []
      F.Seq formats -> concat <$> mapM (go sizes) formats
      F.Loop i n body -> do
        n <- runFormatExpr n
        go (M.insert i n sizes) body

runMainInput :: (MonadState (M.Map String VarName) m, MonadAlpha m, MonadError Error m) => F.IOFormat -> [(S.Set VarName, Statement)] -> m [Statement]
runMainInput format decls = do
  let go initialized = \case
        F.Exp e -> do
          (x, _) <- F.unpackSubscriptedVar e
          y <- lookup' x
          e <- runFormatExpr e
          let decls' = map snd $ filter (\(deps, _) -> not (deps `S.isSubsetOf` initialized) && deps `S.isSubsetOf` S.insert y initialized) decls
          return (cinStatement e : decls', S.insert y initialized)
        F.Newline -> return ([], initialized)
        F.Seq [] -> return ([], initialized)
        F.Seq (format : formats) -> do
          (stmts, initialized) <- go initialized format
          (stmts', initialized) <- go initialized (F.Seq formats)
          return (stmts ++ stmts', initialized)
        F.Loop i n body -> do
          j <- renameVarName LoopCounterNameKind i
          modify' $ M.insert i j
          n <- runFormatExpr n
          (body, initialized) <- go initialized body
          return ([forStatement j n body], initialized)
  let decls' = map snd $ filter (\(deps, _) -> S.null deps) decls
  stmts <- fst <$> go S.empty (F.inputTree format)
  return $ decls' ++ stmts

runMainSolve :: (MonadState (M.Map String VarName) m, MonadAlpha m, MonadError Error m) => F.IOFormat -> m Statement
runMainSolve format = do
  args <- mapM lookup' (F.inputVariables format)
  let solve = Call (Function "solve" []) (map Var args)
  case F.outputVariables format of
    Left x -> do
      y <- renameVarName LocalNameKind x
      modify' $ M.insert x y
      return $ Declare TyAuto y (Just solve)
    Right xs -> do
      ys <- mapM (renameVarName LocalNameKind) xs
      modify' $ \env -> foldl (\env (x, y) -> M.insert x y env) env (zip xs ys)
      return $ DeclareDestructure ys solve

runMainOutput :: (MonadState (M.Map String VarName) m, MonadAlpha m, MonadError Error m) => F.IOFormat -> m [Statement]
runMainOutput format = go (F.outputTree format)
  where
    go = \case
      F.Exp e -> do
        e <- runFormatExpr e
        return [coutStatement e]
      F.Newline -> return [coutStatement (Lit (LitChar '\n'))]
      F.Seq formats -> concat <$> mapM go formats
      F.Loop i n body -> do
        j <- renameVarName LoopCounterNameKind i
        modify' $ M.insert i j
        n <- runFormatExpr n
        body <- go body
        return [forStatement j n body]

runMain :: (MonadAlpha m, MonadError Error m) => F.IOFormat -> m ToplevelStatement
runMain format = do
  (`evalStateT` M.empty) $ do
    decls <- runMainDeclare format
    input <- runMainInput format decls
    solve <- runMainSolve format
    output <- runMainOutput format
    return $ FunDef TyInt "main" [] (input ++ [solve] ++ output)

run :: (MonadAlpha m, MonadError Error m) => Program -> F.IOFormat -> m Program
run prog format = wrapError' "Jikka.CPlusPlus.Convert.AddMain" $ do
  main <- runMain format
  return $ Program (decls prog ++ [main])
