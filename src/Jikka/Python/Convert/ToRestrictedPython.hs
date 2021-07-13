{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Python.Convert.ToRestrictedPython
-- Description : converts AST of the standard Python to AST of our restricted Python. / 標準の Python の抽象構文木を我々の restricted Python の抽象構文木に変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.Python.Convert.ToRestrictedPython
  ( run,
  )
where

import Control.Monad.Except
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Common.Location
import qualified Jikka.Python.Language.Expr as X
import qualified Jikka.RestrictedPython.Language.Expr as Y
import qualified Jikka.RestrictedPython.Language.Util as Y (genType)

-- ---------------------------------------------------------------------------
-- convert AST

runIdent :: X.Ident' -> Y.VarName'
runIdent (WithLoc loc (X.Ident x)) = WithLoc' (Just loc) (Y.VarName x)

runAttribute :: X.Ident' -> Y.Attribute'
runAttribute (WithLoc loc (X.Ident x)) = WithLoc' (Just loc) (Y.UnresolvedAttribute (Y.AttributeName x))

runType :: (MonadAlpha m, MonadError Error m) => X.Type' -> m Y.Type
runType t = wrapAt (loc t) $ case value t of
  X.Constant (X.ConstString _) -> Y.genType
  X.Name (WithLoc _ (X.Ident "None")) -> return $ Y.TupleTy []
  X.Name (WithLoc _ (X.Ident "int")) -> return Y.IntTy
  X.Name (WithLoc _ (X.Ident "bool")) -> return Y.BoolTy
  X.Subscript (WithLoc _ (X.Name (WithLoc _ (X.Ident f)))) e -> case (f, e) of
    ("List", _) -> Y.ListTy <$> runType e
    ("Iterator", _) -> Y.ListTy <$> runType e
    ("Sequence", _) -> Y.ListTy <$> runType e
    ("Tuple", WithLoc _ (X.Tuple es)) -> Y.TupleTy <$> mapM runType es
    ("Tuple", _) -> Y.TupleTy . (: []) <$> runType e
    ("Callable", WithLoc _ (X.Tuple [WithLoc _ (X.List es), e])) -> do
      ts <- mapM runType es
      t <- runType e
      return $ Y.CallableTy ts t
    _ -> throwSemanticError ("not a type: " ++ show t)
  _ -> throwSemanticError ("not a type: " ++ show t)

runMaybeType :: (MonadAlpha m, MonadError Error m) => Maybe X.Type' -> m Y.Type
runMaybeType Nothing = Y.genType
runMaybeType (Just t) = runType t

runConstant :: MonadError Error m => X.Constant -> m Y.Constant
runConstant = \case
  X.ConstNone -> return Y.ConstNone
  X.ConstInt n -> return $ Y.ConstInt n
  X.ConstBool p -> return $ Y.ConstBool p
  e -> throwSemanticError ("unsupported constant: " ++ show e)

runTargetName :: (MonadAlpha m, MonadError Error m) => X.Expr' -> m Y.VarName'
runTargetName e = case value e of
  X.Name x -> return $ runIdent x
  _ -> throwSemanticErrorAt (loc e) ("not an assignment target: " ++ show e)

runTarget :: (MonadAlpha m, MonadError Error m) => X.Expr' -> m Y.Target'
runTarget e =
  WithLoc' (Just (loc e)) <$> case value e of
    X.Subscript f index -> Y.SubscriptTrg <$> runTarget f <*> runExpr index
    X.Name _ -> Y.NameTrg <$> runTargetName e
    X.Tuple es -> Y.TupleTrg <$> mapM runTarget es
    _ -> throwSemanticErrorAt (loc e) ("not an assignment target: " ++ show e)

runTargetIdent :: MonadError Error m => X.Expr' -> m Y.VarName'
runTargetIdent e = case value e of
  X.Name x -> return $ runIdent x
  _ -> throwSemanticErrorAt (loc e) ("not an simple assignment target: " ++ show e)

runComprehension :: (MonadAlpha m, MonadError Error m) => [X.Comprehension] -> m Y.Comprehension
runComprehension = \case
  [comp] -> do
    x <- runTarget (X.compTarget comp)
    iter <- runExpr (X.compIter comp)
    ifs <- mapM runExpr (X.compIfs comp)
    return $ Y.Comprehension x iter ifs
  comp -> throwSemanticError ("many comprehensions are unsupported: " ++ show comp)

runArguments :: (MonadAlpha m, MonadError Error m) => X.Arguments -> m [(Y.VarName', Y.Type)]
runArguments = \case
  X.Arguments
    { X.argsPosonlyargs = [],
      X.argsArgs = args,
      X.argsVarargs = Nothing,
      X.argsKwonlyargs = [],
      X.argsKwDefaults = [],
      X.argsKwarg = Nothing,
      X.argsDefaults = []
    } -> do
      forM args $ \(x, t) -> do
        let x' = runIdent x
        t <- runMaybeType t
        return (x', t)
  args -> throwSemanticError ("unsupported arguments: " ++ show args)

runCompareExpr :: (MonadAlpha m, MonadError Error m) => X.Expr' -> [(X.CmpOp, X.Expr')] -> m Y.Expr
runCompareExpr e1 ops = value' <$> (runExpr e1 >>= (`go` ops))
  where
    withLoc = WithLoc' (Just (loc e1))
    go :: (MonadAlpha m, MonadError Error m) => Y.Expr' -> [(X.CmpOp, X.Expr')] -> m Y.Expr'
    go e1 = \case
      [] -> return . withLoc $ Y.Constant (Y.ConstBool True)
      [(op, e2)] -> withLoc <$> (Y.Compare e1 <$> (Y.CmpOp' op <$> Y.genType) <*> runExpr e2)
      (op, e2) : ops -> do
        t <- Y.genType
        e2 <- runExpr e2
        cont <- go e2 ops
        return . withLoc $ Y.BoolOp (withLoc (Y.Compare e1 (Y.CmpOp' op t) e2)) Y.And cont

runExpr :: (MonadAlpha m, MonadError Error m) => X.Expr' -> m Y.Expr'
runExpr e =
  WithLoc' (Just (loc e)) <$> case value e of
    X.BoolOp e1 op e2 -> Y.BoolOp <$> runExpr e1 <*> return op <*> runExpr e2
    X.BinOp e1 op e2 -> Y.BinOp <$> runExpr e1 <*> return op <*> runExpr e2
    X.UnaryOp op e -> Y.UnaryOp op <$> runExpr e
    X.Lambda args body -> Y.Lambda <$> runArguments args <*> runExpr body
    X.IfExp e1 e2 e3 -> Y.IfExp <$> runExpr e1 <*> runExpr e2 <*> runExpr e3
    X.ListComp e comp -> Y.ListComp <$> runExpr e <*> runComprehension comp
    X.GeneratorExp e comp -> Y.ListComp <$> runExpr e <*> runComprehension comp
    X.Compare e1 e2 -> runCompareExpr e1 e2
    X.Call f args [] -> Y.Call <$> runExpr f <*> mapM runExpr args
    X.Constant const -> Y.Constant <$> runConstant const
    X.Attribute e x -> Y.Attribute <$> runExpr e <*> pure (runAttribute x)
    X.Subscript e1 e2 -> case value e2 of
      X.Slice from to step -> Y.SubscriptSlice <$> runExpr e1 <*> mapM runExpr from <*> mapM runExpr to <*> mapM runExpr step
      _ -> Y.Subscript <$> runExpr e1 <*> runExpr e2
    X.Name x -> return $ Y.Name (runIdent x)
    X.List es -> Y.List <$> Y.genType <*> mapM runExpr es
    X.Tuple es -> Y.Tuple <$> mapM runExpr es
    _ -> throwSemanticErrorAt (loc e) ("unsupported expr: " ++ show e)

runStatement :: (MonadAlpha m, MonadError Error m) => X.Statement' -> m [Y.Statement]
runStatement stmt = wrapAt (loc stmt) $ case value stmt of
  X.FunctionDef _ _ _ _ _ -> throwSemanticError "def statement is not allowed in def statement"
  X.AsyncFunctionDef _ _ _ _ _ -> throwSemanticError "async-def statement is not allowed in def statement"
  X.ClassDef _ _ _ _ _ -> throwSemanticError "class statement is not allowed in def statement"
  X.Return e -> do
    e <- case e of
      Nothing -> return . WithLoc' (Just (loc stmt)) $ Y.Constant Y.ConstNone
      Just e -> runExpr e
    return [Y.Return e]
  X.Delete _ -> throwSemanticErrorAt (loc stmt) "del statement is not allowed in def statement"
  X.Assign xs e -> case xs of
    [] -> return []
    [x] -> do
      x <- runTarget x
      t <- Y.genType
      e <- runExpr e
      return [Y.AnnAssign x t e]
    _ -> throwSemanticError "assign statement with multiple targets is not allowed in def statement"
  X.AugAssign x op e -> do
    x <- runTarget x
    e <- runExpr e
    return [Y.AugAssign x op e]
  X.AnnAssign x t e -> case e of
    Nothing -> throwSemanticError "annotated assignment statement without value is not allowed in def statement"
    Just e -> do
      x <- runTargetIdent x
      t <- runType t
      e <- runExpr e
      return [Y.AnnAssign (WithLoc' (loc' x) (Y.NameTrg x)) t e]
  X.For x e body orelse -> do
    x <- runTarget x
    e <- runExpr e
    body <- runStatements body
    orelse <- runStatements orelse
    return $ Y.For x e body : orelse
  X.AsyncFor _ _ _ _ -> throwSemanticError "async-for statement is not allowed in def statement"
  X.While _ _ _ -> throwSemanticError "while statement is not allowed in def statement"
  X.If e body1 body2 -> do
    e <- runExpr e
    body1 <- runStatements body1
    body2 <- runStatements body2
    return [Y.If e body1 body2]
  X.With _ _ -> throwSemanticError "with statement is not allowed in def statement"
  X.AsyncWith _ _ -> throwSemanticError "async-with statement is not allowed in def statement"
  X.Raise _ _ -> throwSemanticError "raise statement is not allowed in def statement"
  X.Try _ _ _ _ -> throwSemanticError "try statement is not allowed in def statement"
  X.Assert e _ -> do
    e <- runExpr e
    return [Y.Assert e]
  X.Import _ -> throwSemanticError "import statement is not allowed in def statement"
  X.ImportFrom _ _ -> throwSemanticError "import-from statement is not allowed in def statement"
  X.Global _ -> throwSemanticError "global statement is not allowed in def statement"
  X.Nonlocal _ -> throwSemanticError "nonlocal statement is not allowed in def statement"
  X.Expr' _ -> throwSemanticError "expression statement is not allowed in def statement"
  X.Pass -> return []
  X.Break -> throwSemanticError "break statement is not allowed in def statement"
  X.Continue -> throwSemanticError "continue statement is not allowed in def statement"

runStatements :: (MonadAlpha m, MonadError Error m) => [X.Statement'] -> m [Y.Statement]
runStatements stmts = do
  stmts <- mapM (catchError' . runStatement) stmts
  concat <$> reportErrors stmts

runToplevelStatement :: (MonadAlpha m, MonadError Error m) => X.Statement' -> m [Y.ToplevelStatement]
runToplevelStatement stmt = wrapAt (loc stmt) $ case value stmt of
  X.FunctionDef f args body decorators ret -> case decorators of
    [] -> do
      let f' = runIdent f
      args <- runArguments args
      body <- runStatements body
      ret <- runMaybeType ret
      return [Y.ToplevelFunctionDef f' args ret body]
    _ -> throwSemanticError "def statement with decorators is not allowed at toplevel"
  X.AsyncFunctionDef _ _ _ _ _ -> throwSemanticError "async-def statement is not allowed at toplevel"
  X.ClassDef _ _ _ _ _ -> throwSemanticError "class statement is not allowed at toplevel"
  X.Return _ -> throwSemanticError "retrun statement is not allowed at toplevel"
  X.Delete _ -> throwSemanticError "del statement is not allowed at toplevel"
  X.Assign xs e -> case xs of
    [] -> return []
    [x] -> do
      x <- runTargetIdent x
      t <- Y.genType
      e <- runExpr e
      return [Y.ToplevelAnnAssign x t e]
    _ -> throwSemanticError "assignment statement with multiple targets is not allowed at toplevel"
  X.AugAssign _ _ _ -> throwSemanticError "augumented assignment statement is not allowed at toplevel"
  X.AnnAssign x t e -> case e of
    Nothing -> throwSemanticError "annotated assignment statement without value is not allowed at toplevel"
    Just e -> do
      x <- runTargetIdent x
      t <- runType t
      e <- runExpr e
      return [Y.ToplevelAnnAssign x t e]
  X.For _ _ _ _ -> throwSemanticError "for statement is not allowed at toplevel"
  X.AsyncFor _ _ _ _ -> throwSemanticError "async-for statement is not allowed at toplevel"
  X.While _ _ _ -> throwSemanticError "while statement is not allowed at toplevel"
  X.If _ _ _ -> throwSemanticError "if statement is not allowed at toplevel"
  X.With _ _ -> throwSemanticError "with statement is not allowed at toplevel"
  X.AsyncWith _ _ -> throwSemanticError "async-with statement is not allowed at toplevel"
  X.Raise _ _ -> throwSemanticError "raise statement is not allowed at toplevel"
  X.Try _ _ _ _ -> throwSemanticError "try statement is not allowed at toplevel"
  X.Assert e _ -> do
    e <- runExpr e
    return [Y.ToplevelAssert e]
  X.Import _ -> return []
  X.ImportFrom _ _ -> return []
  X.Global _ -> throwSemanticError "global statement is not allowed at toplevel"
  X.Nonlocal _ -> throwSemanticError "nonlocal statement is not allowed at toplevel"
  X.Expr' _ -> throwSemanticError "expression statement is not allowed at toplevel"
  X.Pass -> return []
  X.Break -> throwSemanticError "break statement is not allowed at toplevel"
  X.Continue -> throwSemanticError "continue statement is not allowed at toplevel"

runProgram :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
runProgram stmts = do
  stmts <- mapM (catchError' . runToplevelStatement) stmts
  concat <$> reportErrors stmts

run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = wrapError' "Failed at Jikka.Python.Convert.ToplevelDecl" $ runProgram prog
