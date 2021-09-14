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
import Jikka.RestrictedPython.Language.Util

type MainFunction = (Maybe Loc, [(VarName', Type)], Type, [Statement])

splitMain :: Program -> (Maybe MainFunction, Program)
splitMain = \case
  [] -> (Nothing, [])
  ToplevelFunctionDef (WithLoc' loc (VarName (Just "main") Nothing)) args ret body : stmts -> (Just (loc, args, ret, body), stmts)
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

pattern IntInput <-
  CallBuiltin (BuiltinInt _) [CallBuiltin BuiltinInput []]

pattern MapIntInputSplit <-
  CallBuiltin
    (BuiltinMap [_] _)
    [ WithLoc' _ (Constant (ConstBuiltin (BuiltinInt _))),
      CallMethod
        (CallBuiltin BuiltinInput [])
        (WithLoc' _ BuiltinSplit)
        []
      ]

pattern ListMapIntInputSplit <-
  CallBuiltin
    (BuiltinList _)
    [ CallBuiltin
        (BuiltinMap [_] _)
        [ WithLoc' _ (Constant (ConstBuiltin (BuiltinInt _))),
          CallMethod
            (CallBuiltin BuiltinInput [])
            (WithLoc' _ BuiltinSplit)
            []
          ]
      ]

pattern ListRange n <-
  CallBuiltin
    (BuiltinList _)
    [CallBuiltin BuiltinRange1 [WithLoc' _ (Name (WithLoc' _ n))]]

parseAnnAssign :: (MonadAlpha m, MonadError Error m) => Target' -> Type -> Expr' -> [Statement] -> m (FormatTree, Maybe ([String], Either String [String]), [Statement])
parseAnnAssign x _ e cont = do
  let subscriptTrg x = case value' x of
        NameTrg x -> return (formatVarName (value' x), [])
        SubscriptTrg x (WithLoc' _ (Name i)) -> second (++ [formatVarName (value' i)]) <$> subscriptTrg x
        _ -> throwSemanticErrorAt' (loc' x) $ "name target or subscript target is expected, but got: " ++ formatTarget x
  let subscriptTupleTrg x = case value' x of
        TupleTrg xs -> mapM subscriptTrg xs
        _ -> throwSemanticErrorAt' (loc' x) $ "tuple target is expected, but got: " ++ formatTarget x
  let nameTrg x = case value' x of
        NameTrg x -> return $ formatVarName (value' x)
        _ -> throwSemanticErrorAt' (loc' x) $ "name target is expected, but got: " ++ formatTarget x
  let nameOrTupleTrg x = case value' x of
        NameTrg x -> return . Left $ formatVarName (value' x)
        TupleTrg xs -> Right <$> mapM nameTrg xs
        _ -> throwSemanticErrorAt' (loc' x) $ "name target or tuple target is expected, but got: " ++ formatTarget x
  let nameExpr e = case value' e of
        Name x -> return $ formatVarName (value' x)
        _ -> throwSemanticErrorAt' (loc' e) $ "variable is expected, but got: " ++ formatExpr e
  case e of
    -- int(input())
    IntInput -> do
      (x, indices) <- subscriptTrg x
      return (Seq [packSubscriptedVar' x indices, Newline], Nothing, cont)
    -- map(int, input().split())
    MapIntInputSplit -> do
      outputs <- subscriptTupleTrg x
      return (Seq (map (uncurry packSubscriptedVar') outputs ++ [Newline]), Nothing, cont)
    -- list(map(int, input().split()))
    ListMapIntInputSplit -> do
      (x, indices) <- subscriptTrg x
      case cont of
        Assert (WithLoc' _ (Compare (CallBuiltin (BuiltinLen _) [WithLoc' _ (Name x')]) (CmpOp' Eq' _) n)) : cont | formatVarName (value' x') == x -> do
          i <- formatVarName . value' <$> genVarName'
          n <- nameExpr n
          return (Seq [Loop i (Var n) (Exp (At (packSubscriptedVar x indices) i)), Newline], Nothing, cont)
        _ -> throwSemanticErrorAt' (loc' e) "after `xs = list(map(int, input().split()))', we need to write `assert len(xs) == n`"
    -- list(range(n))
    ListRange n -> do
      let isListRange = \case
            AnnAssign _ _ (ListRange n') | n' == n -> True
            _ -> False
      cont <- return $ dropWhile isListRange cont
      case cont of
        For _ (CallBuiltin BuiltinRange1 [WithLoc' _ (Name n')]) _ : _ | value' n' == n -> return (Seq [], Nothing, cont) -- TODO: add more strict checks
        _ -> throwSemanticErrorAt' (loc' e) "after some repetition of `xs = list(range(n))', we need to write `for i in range(n):`"
    -- solve(...)
    WithLoc' _ (Call (WithLoc' _ (Name (WithLoc' _ (VarName (Just "solve") Nothing)))) args) -> do
      inputs <- mapM nameExpr args
      output <- nameOrTupleTrg x
      return (Seq [], Just (inputs, output), cont)
    _ -> throwSemanticErrorAt' (loc' e) "assignments in main function must be `x = int(input())', `x, y, z = map(int, input().split())', `xs = list(map(int, input().split()))', `xs = list(range(n))' or `x, y, z = solve(a, b, c)'"

parseFor :: MonadError Error m => ([Statement] -> m (FormatTree, Maybe ([String], Either String [String]), FormatTree)) -> Target' -> Expr' -> [Statement] -> m (FormatTree, FormatTree)
parseFor go x e body = do
  x <- case value' x of
    NameTrg x -> return x
    _ -> throwSemanticErrorAt' (loc' x) $ "for loops in main function must use `range' like `for i in range(n): ...'" ++ formatTarget x
  n <- case e of
    CallBuiltin BuiltinRange1 [n] -> return n
    _ -> throwSemanticErrorAt' (loc' e) $ "for loops in main function must use `range' like `for i in range(n): ...': " ++ formatExpr e
  n <- case value' n of
    Name n -> return $ Right (n, 0)
    BinOp (WithLoc' _ (Name n)) Add (WithLoc' _ (Constant (ConstInt k))) -> return $ Right (n, k)
    BinOp (WithLoc' _ (Name n)) Sub (WithLoc' _ (Constant (ConstInt k))) -> return $ Right (n, - k)
    Call (WithLoc' _ (Constant (ConstBuiltin (BuiltinLen _)))) [WithLoc' _ (Name xs)] -> return $ Left xs
    _ -> throwSemanticErrorAt' (loc' n) $ "for loops in main function must use `range(x)', `range(x + k)', `range(x - k)', `range(len(xs))`: " ++ formatExpr n
  n <- return $ case n of
    Right (n, k) ->
      let n' = Var (formatVarName (value' n))
       in if k == 0 then n' else Plus n' k
    Left xs -> Len (Var (formatVarName (value' xs)))
  (input, solve, output) <- go body
  when (isJust solve) $ do
    throwSemanticError "cannot call `solve(...)' in for loop"
  let x' = formatVarName (value' x)
  return (Loop x' n input, Loop x' n output)

parseExprStatement :: (MonadAlpha m, MonadError Error m) => Expr' -> m FormatTree
parseExprStatement e = do
  let subscriptExpr e = case value' e of
        Name x -> return (formatVarName (value' x), [])
        Subscript e (WithLoc' _ (Name i)) -> second (++ [formatVarName (value' i)]) <$> subscriptExpr e
        _ -> throwSemanticErrorAt' (loc' e) $ "subscripted variable is expected, but got: " ++ formatExpr e
  let starredExpr e = do
        (e, starred) <- return $ case value' e of
          Starred e -> (e, True)
          _ -> (e, False)
        (x, indices) <- subscriptExpr e
        return (x, indices, starred)
  let pack (x, indices, starred)
        | not starred = return $ packSubscriptedVar' x indices
        | otherwise = do
          let xs = packSubscriptedVar x indices
          i <- formatVarName . value' <$> genVarName'
          return $ Loop i (Len xs) (packSubscriptedVar' x (indices ++ [i]))
  case e of
    CallBuiltin (BuiltinPrint _) args -> do
      outputs <- mapM starredExpr args
      outputs <- mapM pack outputs
      return $ Seq (outputs ++ [Newline])
    _ -> throwSemanticErrorAt' (loc' e) "only `print(...)' is allowed for expr statements in main function"

parseMain :: (MonadAlpha m, MonadError Error m) => MainFunction -> m IOFormat
parseMain (loc, _, _, body) = wrapAt' loc $ pack =<< go [] body
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
    go :: (MonadAlpha m, MonadError Error m) => [(FormatTree, Maybe ([String], Either String [String]), FormatTree)] -> [Statement] -> m (FormatTree, Maybe ([String], Either String [String]), FormatTree)
    go formats = \case
      Return _ : _ -> throwSemanticError "return statement is not allowd in main function"
      AugAssign _ _ _ : _ -> throwSemanticError "augumented assignment statement is not allowd in main function"
      AnnAssign x t e : cont -> do
        (inputs, solve, cont) <- parseAnnAssign x t e cont
        go (formats ++ [(inputs, solve, Seq [])]) cont
      For x e body : cont -> do
        (inputs, outputs) <- parseFor (go []) x e body
        go (formats ++ [(inputs, Nothing, outputs)]) cont
      If _ _ _ : _ -> throwSemanticError "if statement is not allowd in main function"
      Assert _ : _ -> throwSemanticError "assert statement is allowd only after `xs = list(map(int, input().split()))` in main function"
      Expr' e : cont -> do
        output <- parseExprStatement e
        go (formats ++ [(Seq [], Nothing, output)]) cont
      [] -> do
        let input = Seq (map (\(x, _, _) -> x) formats)
        let outputs = Seq (map (\(_, _, z) -> z) formats)
        solve <- case mapMaybe (\(_, y, _) -> y) formats of
          [] -> return Nothing
          [solve] -> return $ Just solve
          _ -> throwSemanticError "cannot call solve function twice"
        return (input, solve, outputs)

run :: (MonadAlpha m, MonadError Error m) => Program -> m (Maybe IOFormat, Program)
run prog = wrapError' "Jikka.RestrictedPython.Convert.ParseMain" $ do
  (main, prog) <- return $ splitMain prog
  main <- forM main $ \main -> do
    checkMainType main
    main <- parseMain main
    return $ normalizeIOFormat main
  return (main, prog)
