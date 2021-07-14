{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Jikka.RestrictedPython.Convert.ParseMain
-- Description : analyze @main@ function into input formats. / @main@ 関数を分析して入力フォーマットを得ます。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.ParseMain
  ( run,
  )
where

import Control.Arrow
import Data.Maybe
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.IOFormat
import Jikka.RestrictedPython.Format (formatExpr, formatTarget)
import Jikka.RestrictedPython.Language.Expr

type MainFunction = (Maybe Loc, [(VarName', Type)], Type, [Statement])

splitMain :: Program -> (Maybe MainFunction, Program)
splitMain = \case
  [] -> (Nothing, [])
  ToplevelFunctionDef (WithLoc' loc (VarName "main")) args ret body : stmts -> (Just (loc, args, ret, body), stmts)
  stmt : stmts -> second (stmt :) $ splitMain stmts

checkMainType :: MonadError Error m => MainFunction -> m ()
checkMainType (loc, args, ret, _) = wrapAt' loc $ case args of
  _ : _ -> throwTypeError "main function must not take arguments"
  [] -> case ret of
    VarTy _ -> return ()
    NoneTy -> return ()
    _ -> throwTypeError "main function must return None"

pattern CallBuiltin b args <- WithLoc' _ (Call (WithLoc' _ (Constant (ConstBuiltin b))) args)

pattern CallMethod e a args <- WithLoc' _ (Call (WithLoc' _ (Attribute e a)) args)

parseAnnAssign :: MonadError Error m => Target' -> Type -> Expr' -> m (FormatTree, Maybe ([String], Either String [String]), FormatTree)
parseAnnAssign x _ e = do
  let subscriptTrg x = case value' x of
        NameTrg x -> return (unVarName (value' x), [])
        SubscriptTrg x (WithLoc' _ (Name i)) -> second (++ [unVarName (value' i)]) <$> subscriptTrg x
        _ -> throwSemanticErrorAt' (loc' x) $ "name target or subscript target is expected, but got: " ++ formatTarget x
  let subscriptTupleTrg x = case value' x of
        TupleTrg xs -> mapM subscriptTrg xs
        _ -> throwSemanticErrorAt' (loc' x) $ "tuple target is expected, but got: " ++ formatTarget x
  let nameTrg x = case value' x of
        NameTrg x -> return $ unVarName (value' x)
        _ -> throwSemanticErrorAt' (loc' x) $ "name target is expected, but got: " ++ formatTarget x
  let nameOrTupleTrg x = case value' x of
        NameTrg x -> return . Left $ unVarName (value' x)
        TupleTrg xs -> Right <$> mapM nameTrg xs
        _ -> throwSemanticErrorAt' (loc' x) $ "name target or tuple target is expected, but got: " ++ formatTarget x
  let nameExpr e = case value' e of
        Name x -> return $ unVarName (value' x)
        _ -> throwSemanticErrorAt' (loc' e) $ "variable is expected, but got: " ++ formatExpr e
  case e of
    -- int(input())
    CallBuiltin
      (BuiltinInt StringTy)
      [CallBuiltin BuiltinInput []] -> do
        (x, indices) <- subscriptTrg x
        return (Seq [], Nothing, Seq [packSubscriptedVar' x indices, Newline])
    -- map(int, input().split())
    ( CallBuiltin
        (BuiltinMap [StringTy] IntTy)
        [ WithLoc' _ (Constant (ConstBuiltin (BuiltinInt StringTy))),
          CallMethod
            (CallBuiltin BuiltinInput [])
            (WithLoc' _ BuiltinSplit)
            []
          ]
      ) -> do
        outputs <- subscriptTupleTrg x
        return (Seq [], Nothing, Seq (map (uncurry packSubscriptedVar') outputs ++ [Newline]))
    -- list(map(int, input().split()))
    CallBuiltin
      (BuiltinList IntTy)
      [ CallBuiltin
          (BuiltinMap [StringTy] IntTy)
          [ WithLoc' _ (Constant (ConstBuiltin (BuiltinInt StringTy))),
            CallMethod
              (CallBuiltin BuiltinInput [])
              (WithLoc' _ BuiltinSplit)
              []
            ]
        ] -> do
        (x, indices) <- subscriptTrg x
        return (Seq [], Nothing, Seq [packSubscriptedVar' x indices, Newline])
    -- solve(...)
    WithLoc' _ (Call (WithLoc' _ (Name (WithLoc' _ (VarName "solve")))) args) -> do
      inputs <- mapM nameExpr args
      output <- nameOrTupleTrg x
      return (Seq [], Just (inputs, output), Seq [])
    _ -> throwSemanticErrorAt' (loc' e) $ "assingments in main function must be `x = int(input())', `x, y, z = map(int, input().split())', `xs = list(map(int, input().split()))' or `x, y, z = solve(a, b, c)': " ++ formatExpr e

parseFor :: MonadError Error m => ([Statement] -> m (FormatTree, Maybe ([String], Either String [String]), FormatTree)) -> Target' -> Expr' -> [Statement] -> m (FormatTree, FormatTree)
parseFor go x e body = do
  x <- case value' x of
    NameTrg x -> return x
    _ -> throwSemanticErrorAt' (loc' x) $ "for loops in main function must use `range' like `for i in range(n): ...'" ++ formatTarget x
  n <- case e of
    CallBuiltin BuiltinRange1 [n] -> return n
    _ -> throwSemanticErrorAt' (loc' e) $ "for loops in main function must use `range' like `for i in range(n): ...': " ++ formatExpr e
  (n, k) <- case value' n of
    Name n -> return (n, 0)
    BinOp (WithLoc' _ (Name n)) Add (WithLoc' _ (Constant (ConstInt k))) -> return (n, k)
    BinOp (WithLoc' _ (Name n)) Sub (WithLoc' _ (Constant (ConstInt k))) -> return (n, - k)
    _ -> throwSemanticErrorAt' (loc' n) $ "for loops in main function must use `range(x)', `range(x + k)' or `range(x - k)'" ++ formatExpr n
  (input, solve, output) <- go body
  when (isJust solve) $ do
    throwSemanticError "cannot call `solve(...)' in for loop"
  let x' = unVarName (value' x)
  let n' = Var (unVarName (value' n))
  let n'' = if k == 0 then n' else Plus n' k
  return (Loop x' n'' input, Loop x' n'' output)

parseExprStatement :: MonadError Error m => Expr' -> m FormatTree
parseExprStatement e = do
  let subscriptExpr e = case value' e of
        Name x -> return (unVarName (value' x), [])
        Subscript e (WithLoc' _ (Name i)) -> second (++ [unVarName (value' i)]) <$> subscriptExpr e
        _ -> throwSemanticErrorAt' (loc' e) $ "subscripted variable is expected, but got: " ++ formatExpr e
  case e of
    CallBuiltin (BuiltinPrint _) args -> do
      outputs <- mapM subscriptExpr args
      return $ Seq (map (uncurry packSubscriptedVar') outputs ++ [Newline])
    _ -> throwSemanticErrorAt' (loc' e) "only `print(...)' is allowed for expr statements in main function"

parseMain :: MonadError Error m => MainFunction -> m IOFormat
parseMain (loc, _, _, body) = wrapAt' loc $ pack =<< go body
  where
    pack :: MonadError Error m => (FormatTree, Maybe ([String], Either String [String]), FormatTree) -> m IOFormat
    pack (_, Nothing, _) = throwSemanticError "main function must call solve function"
    pack (inputTree, Just (inputVariables, outputVariables), outputTree) =
      return $
        IOFormat
          { inputTree = inputTree,
            inputVariables = inputVariables,
            outputVariables = outputVariables,
            outputTree = outputTree
          }
    go :: MonadError Error m => [Statement] -> m (FormatTree, Maybe ([String], Either String [String]), FormatTree)
    go stmts = do
      formats <- mapM go' stmts
      let input = Seq (map (\(x, _, _) -> x) formats)
      let outputs = Seq (map (\(_, _, z) -> z) formats)
      solve <- case mapMaybe (\(_, y, _) -> y) formats of
        [] -> return Nothing
        [solve] -> return $ Just solve
        _ -> throwSemanticError "cannot call solve function twice"
      return (input, solve, outputs)
    go' :: MonadError Error m => Statement -> m (FormatTree, Maybe ([String], Either String [String]), FormatTree)
    go' = \case
      Return _ -> throwSemanticError "return statement is not allowd in main function"
      AugAssign _ _ _ -> throwSemanticError "augumented assignment statement is not allowd in main function"
      AnnAssign x t e -> parseAnnAssign x t e
      For x e body -> do
        (inputs, outputs) <- parseFor go x e body
        return (inputs, Nothing, outputs)
      If _ _ _ -> throwSemanticError "if statement is not allowd in main function"
      Assert _ -> throwSemanticError "assert statement is not allowd in main function"
      Expr' e -> do
        output <- parseExprStatement e
        return (Seq [], Nothing, output)

run :: (MonadAlpha m, MonadError Error m) => Program -> m (Maybe IOFormat, Program)
run prog = wrapError' "Jikka.RestrictedPython.Convert.ParseMain" $ do
  (main, prog) <- return $ splitMain prog
  main <- forM main $ \main -> do
    checkMainType main
    parseMain main
  return (main, prog)
