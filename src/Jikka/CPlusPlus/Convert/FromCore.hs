{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.FromCore
-- Description : converts core exprs to C++ exprs.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.CPlusPlus.FromCore` converts exprs of our core language to exprs of C++.
module Jikka.CPlusPlus.Convert.FromCore
  ( run,
  )
where

import Data.Char (isAlphaNum)
import Data.List (intercalate)
import qualified Jikka.CPlusPlus.Format as Y (formatExpr)
import qualified Jikka.CPlusPlus.Language.Expr as Y
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Format as X (formatBuiltinIsolated, formatType)
import qualified Jikka.Core.Language.Beta as X
import qualified Jikka.Core.Language.BuiltinPatterns as X
import qualified Jikka.Core.Language.Expr as X
import qualified Jikka.Core.Language.TypeCheck as X

--------------------------------------------------------------------------------
-- monad

data NameKind
  = LocalNameKind
  | LocalArgumentNameKind
  | ConstantNameKind
  | FunctionNameKind
  | ArgumentNameKind
  deriving (Eq, Ord, Show, Read)

fromNameKind :: NameKind -> String
fromNameKind = \case
  LocalNameKind -> "x"
  LocalArgumentNameKind -> "b"
  ConstantNameKind -> "c"
  FunctionNameKind -> "f"
  ArgumentNameKind -> "a"

newFreshName :: MonadAlpha m => NameKind -> String -> m Y.VarName
newFreshName kind hint = do
  i <- nextCounter
  let prefix = case takeWhile (\c -> isAlphaNum c || c == '_') hint of
        "" -> fromNameKind kind
        hint' -> hint' ++ "_"
  return (Y.VarName (prefix ++ show i))

renameVarName :: MonadAlpha m => NameKind -> X.VarName -> m Y.VarName
renameVarName kind x = newFreshName kind (X.unVarName x)

type Env = [(X.VarName, X.Type, Y.VarName)]

typecheckExpr :: MonadError Error m => Env -> X.Expr -> m X.Type
typecheckExpr env = X.typecheckExpr (map (\(x, t, _) -> (x, t)) env)

lookupVarName :: MonadError Error m => Env -> X.VarName -> m Y.VarName
lookupVarName env x = case lookup x (map (\(x, _, y) -> (x, y)) env) of
  Just y -> return y
  Nothing -> throwInternalError $ "undefined variable: " ++ X.unVarName x

--------------------------------------------------------------------------------
-- run

runType :: MonadError Error m => X.Type -> m Y.Type
runType = \case
  t@X.VarTy {} -> throwInternalError $ "variable type appears at invalid place: " ++ X.formatType t
  X.IntTy -> return Y.TyInt64
  X.BoolTy -> return Y.TyBool
  X.ListTy t -> Y.TyVector <$> runType t
  X.TupleTy ts -> Y.TyTuple <$> mapM runType ts
  X.FunTy ts t -> Y.TyFunction <$> runType t <*> mapM runType ts

runLiteral :: MonadError Error m => X.Literal -> m Y.Expr
runLiteral = \case
  X.LitBuiltin builtin -> throwInternalError $ "cannot use builtin functaions as values: " ++ X.formatBuiltinIsolated builtin
  X.LitInt n -> return $ Y.Lit (Y.LitInt64 n)
  X.LitBool p -> return $ Y.Lit (Y.LitBool p)
  X.LitNil t -> do
    t <- runType t
    return $ Y.Call (Y.Function "std::vector" [t]) []

runAppBuiltin :: MonadError Error m => X.Builtin -> [Y.Expr] -> m Y.Expr
runAppBuiltin f args = case (f, args) of
  -- arithmetical functions
  (X.Negate, [e]) -> return $ Y.UnOp Y.Negate e
  (X.Plus, [e1, e2]) -> return $ Y.BinOp Y.Add e1 e2
  (X.Minus, [e1, e2]) -> return $ Y.BinOp Y.Sub e1 e2
  (X.Mult, [e1, e2]) -> return $ Y.BinOp Y.Mul e1 e2
  (X.FloorDiv, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::floordiv" []) [e1, e2]
  (X.FloorMod, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::floormod" []) [e1, e2]
  (X.CeilDiv, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::ceildiv" []) [e1, e2]
  (X.CeilMod, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::ceilmod" []) [e1, e2]
  (X.Pow, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::pow" []) [e1, e2]
  -- induction functions
  (X.NatInd t, [base, step, n]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::natind" [t]) [base, step, n]
  -- advanced arithmetical functions
  (X.Abs, [e]) -> return $ Y.Call (Y.Function "std::abs" []) [e]
  (X.Gcd, [e1, e2]) -> return $ Y.Call (Y.Function "std::gcd" []) [e1, e2]
  (X.Lcm, [e1, e2]) -> return $ Y.Call (Y.Function "std::lcm" []) [e1, e2]
  (X.Min2 t, [e1, e2]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "std::min" [t]) [e1, e2]
  (X.Max2 t, [e1, e2]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "std::max" [t]) [e1, e2]
  -- logical functions
  (X.Not, [e]) -> return $ Y.UnOp Y.Not e
  (X.And, [e1, e2]) -> return $ Y.BinOp Y.And e1 e2
  (X.Or, [e1, e2]) -> return $ Y.BinOp Y.Or e1 e2
  (X.Implies, [e1, e2]) -> return $ Y.BinOp Y.Or (Y.UnOp Y.Not e1) e2
  (X.If _, [e1, e2, e3]) -> return $ Y.Cond e1 e2 e3
  -- bitwise functions
  (X.BitNot, [e]) -> return $ Y.UnOp Y.BitNot e
  (X.BitAnd, [e1, e2]) -> return $ Y.BinOp Y.BitAnd e1 e2
  (X.BitOr, [e1, e2]) -> return $ Y.BinOp Y.BitOr e1 e2
  (X.BitXor, [e1, e2]) -> return $ Y.BinOp Y.BitXor e1 e2
  (X.BitLeftShift, [e1, e2]) -> return $ Y.BinOp Y.BitLeftShift e1 e2
  (X.BitRightShift, [e1, e2]) -> return $ Y.BinOp Y.BitRightShift e1 e2
  -- modular functions
  (X.ModInv, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::modinv" []) [e1, e2]
  (X.ModPow, [e1, e2, e3]) -> return $ Y.Call (Y.Function "jikka::modpow" []) [e1, e2, e3]
  -- list functions
  (X.Cons t, [e1, e2]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::cons" [t]) [e1, e2]
  (X.Foldl t1 t2, [e1, e2, e3]) -> do
    t1 <- runType t1
    t2 <- runType t2
    return $ Y.Call (Y.Function "jikka::foldl" [t1, t2]) [e1, e2, e3]
  (X.Scanl t1 t2, [e1, e2, e3]) -> do
    t1 <- runType t1
    t2 <- runType t2
    return $ Y.Call (Y.Function "jikka::scanl" [t1, t2]) [e1, e2, e3]
  (X.Len _, [e]) -> return $ Y.Cast Y.TyInt64 (Y.Call (Y.Method e "size") [])
  (X.Tabulate t, [n, f]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::tabulate" [t]) [n, f]
  (X.Map t1 t2, [f, xs]) -> do
    t1 <- runType t1
    t2 <- runType t2
    return $ Y.Call (Y.Function "jikka::fmap" [t1, t2]) [f, xs]
  (X.Filter t, [f, xs]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::filter" [t]) [f, xs]
  (X.At _, [e1, e2]) -> return $ Y.At e1 e2
  (X.SetAt t, [e1, e2, e3]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::setat" [t]) [e1, e2, e3]
  (X.Elem t, [e1, e2]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::elem" [t]) [e1, e2]
  (X.Sum, [e]) -> return $ Y.Call (Y.Function "jikka::sum" []) [e]
  (X.Product, [e]) -> return $ Y.Call (Y.Function "jikka::product" []) [e]
  (X.Min1 t, [e]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::minimum" [t]) [e]
  (X.Max1 t, [e]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::maximum" [t]) [e]
  (X.ArgMin t, [e]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::argmin" [t]) [e]
  (X.ArgMax t, [e]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::argmax" [t]) [e]
  (X.All, [e]) -> return $ Y.Call (Y.Function "jikka::all" []) [e]
  (X.Any, [e]) -> return $ Y.Call (Y.Function "jikka::any" []) [e]
  (X.Sorted t, [e]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::sort" [t]) [e]
  (X.List _, [e]) -> return e
  (X.Reversed t, [e]) -> do
    t <- runType t
    return $ Y.Call (Y.Function "jikka::reverse" [t]) [e]
  (X.Range1, [e]) -> return $ Y.Call (Y.Function "jikka::range1" []) [e]
  (X.Range2, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::range2" []) [e1, e2]
  (X.Range3, [e1, e2, e3]) -> return $ Y.Call (Y.Function "jikka::range3" []) [e1, e2, e3]
  -- tuple functions
  (X.Tuple ts, es) -> do
    ts <- mapM runType ts
    return $ Y.Call (Y.Function "std::tuple" ts) es
  (X.Proj _ n, [e]) -> return $ Y.Call (Y.Function (Y.FunName ("std::get<" ++ show n ++ ">")) []) [e]
  -- comparison
  (X.LessThan _, [e1, e2]) -> return $ Y.BinOp Y.LessThan e1 e2
  (X.LessEqual _, [e1, e2]) -> return $ Y.BinOp Y.LessEqual e1 e2
  (X.GreaterThan _, [e1, e2]) -> return $ Y.BinOp Y.GreaterThan e1 e2
  (X.GreaterEqual _, [e1, e2]) -> return $ Y.BinOp Y.GreaterEqual e1 e2
  (X.Equal _, [e1, e2]) -> return $ Y.BinOp Y.Equal e1 e2
  (X.NotEqual _, [e1, e2]) -> return $ Y.BinOp Y.NotEqual e1 e2
  -- combinational functions
  (X.Fact, [e]) -> return $ Y.Call (Y.Function "jikka::fact" []) [e]
  (X.Choose, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::choose" []) [e1, e2]
  (X.Permute, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::permute" []) [e1, e2]
  (X.MultiChoose, [e1, e2]) -> return $ Y.Call (Y.Function "jikka::multichoose" []) [e1, e2]
  _ -> throwInternalError $ "invalid builtin call: " ++ X.formatBuiltinIsolated f ++ "(" ++ intercalate "," (map (fst . Y.formatExpr) args) ++ ")"

runExpr :: (MonadAlpha m, MonadError Error m) => Env -> X.Expr -> m Y.Expr
runExpr env = \case
  X.Var x -> Y.Var <$> lookupVarName env x
  X.Lit lit -> runLiteral lit
  X.App f args -> do
    args <- mapM (runExpr env) args
    case f of
      X.Lit (X.LitBuiltin builtin) -> runAppBuiltin builtin args
      e -> do
        e <- runExpr env e
        return $ Y.Call (Y.Callable e) args
  X.Lam args e -> do
    args <- forM args $ \(x, t) -> do
      y <- renameVarName LocalArgumentNameKind x
      return (x, t, y)
    let env' = reverse args ++ env
    args <- forM args $ \(_, t, y) -> do
      t <- runType t
      return (t, y)
    ret <- runType =<< typecheckExpr env' e
    body <- runExprToStatements env' e
    return $ Y.Lam args ret body
  X.Let x _ e1 e2 -> runExpr env $ X.substitute x e1 e2

runExprToStatements :: (MonadAlpha m, MonadError Error m) => Env -> X.Expr -> m [Y.Statement]
runExprToStatements env = \case
  X.Let x t e1 e2 -> do
    y <- renameVarName LocalNameKind x
    t' <- runType t
    e1 <- runExpr env e1
    e2 <- runExprToStatements ((x, t, y) : env) e2
    return $ Y.Declare t' y (Just e1) : e2
  X.If' _ e1 e2 e3 -> do
    e1 <- runExpr env e1
    e2 <- runExprToStatements env e2
    e3 <- runExprToStatements env e3
    return [Y.If e1 e2 (Just e3)]
  e -> do
    e <- runExpr env e
    return [Y.Return e]

runToplevelFunDef :: (MonadAlpha m, MonadError Error m) => Env -> Y.VarName -> [(X.VarName, X.Type)] -> X.Type -> X.Expr -> m [Y.ToplevelStatement]
runToplevelFunDef env f args ret body = do
  ret <- runType ret
  args <- forM args $ \(x, t) -> do
    y <- renameVarName ArgumentNameKind x
    return (x, t, y)
  body <- runExprToStatements (reverse args ++ env) body
  args <- forM args $ \(_, t, y) -> do
    t <- runType t
    return (t, y)
  return [Y.FunDef ret f args body]

runToplevelVarDef :: (MonadAlpha m, MonadError Error m) => Env -> Y.VarName -> X.Type -> X.Expr -> m [Y.ToplevelStatement]
runToplevelVarDef env x t e = do
  t <- runType t
  e <- runExpr env e
  return [Y.VarDef t x e]

runMainRead :: (MonadAlpha m, MonadError Error m) => Y.VarName -> X.Type -> m [Y.Statement]
runMainRead x = \case
  t@X.VarTy {} -> throwInternalError $ "variable type appears at invalid place: " ++ X.formatType t
  X.IntTy ->
    return
      [ Y.Declare Y.TyInt64 x Nothing,
        Y.ExprStatement (Y.BinOp Y.BitRightShift (Y.Var "std::cin") (Y.Var x))
      ]
  X.BoolTy -> do
    s <- newFreshName LocalNameKind ""
    return
      [ Y.Declare Y.TyString s Nothing,
        Y.ExprStatement (Y.BinOp Y.BitRightShift (Y.Var "std::cin") (Y.Var s)),
        Y.Declare Y.TyBool x (Just (Y.Cond (Y.BinOp Y.NotEqual (Y.Var s) (Y.Lit (Y.LitString "false"))) (Y.Lit (Y.LitBool True)) (Y.Lit (Y.LitBool False))))
      ]
  X.ListTy _ -> throwInternalError "runMainRead TODO" -- TODO
  X.TupleTy _ -> throwInternalError "runMainRead TODO" -- TODO
  t@X.FunTy {} -> throwInternalError $ "cannot print function: " ++ X.formatType t

runMainWrite :: (MonadAlpha m, MonadError Error m) => Y.Expr -> X.Type -> m [Y.Statement]
runMainWrite e = \case
  t@X.VarTy {} -> throwInternalError $ "variable type appears at invalid place: " ++ X.formatType t
  X.IntTy -> return [Y.ExprStatement (Y.BinOp Y.BitLeftShift (Y.Var "std::cout") e)]
  X.BoolTy -> return [Y.ExprStatement (Y.BinOp Y.BitLeftShift (Y.Var "std::cout") e)]
  X.ListTy _ -> throwInternalError "runMainWrite TODO" -- TODO
  X.TupleTy ts -> do
    let open = [Y.ExprStatement (Y.BinOp Y.BitLeftShift (Y.Var "std::cout") (Y.Lit (Y.LitChar '(')))]
    stmts <- forM (zip [0 ..] ts) $ \(i, t) -> do
      let comma = if i == 0 then [] else [Y.ExprStatement (Y.BinOp Y.BitLeftShift (Y.Var "std::cout") (Y.Lit (Y.LitString ", ")))]
      stmts <- runMainWrite (Y.Call (Y.Function (Y.FunName ("std::get<" ++ show i ++ ">")) []) [e]) t
      return $ comma ++ stmts
    let close = [Y.ExprStatement (Y.BinOp Y.BitLeftShift (Y.Var "std::cout") (Y.Lit (Y.LitChar ')')))]
    return $ open ++ concat stmts ++ close
  t@X.FunTy {} -> throwInternalError $ "cannot print function: " ++ X.formatType t

runMain :: (MonadAlpha m, MonadError Error m) => Y.VarName -> X.Type -> m [Y.ToplevelStatement]
runMain solve t = do
  (body, ans, t) <- case t of
    X.FunTy ts ret -> do
      body <- forM ts $ \t -> do
        x <- newFreshName LocalNameKind ""
        stmts <- runMainRead x t
        return (stmts, x)
      let body' = concatMap fst body
      ans <- newFreshName LocalNameKind ""
      let func = Y.Function (Y.FunName (Y.unVarName solve)) []
      let args = map (Y.Var . snd) body
      ret' <- runType ret
      return (body' ++ [Y.Declare ret' ans (Just (Y.Call func args))], ans, ret)
    _ -> return ([], solve, t)
  body' <- runMainWrite (Y.Var ans) t
  let newline = [Y.ExprStatement (Y.BinOp Y.BitLeftShift (Y.Var "std::cout") (Y.Lit (Y.LitChar '\n')))]
  return [Y.FunDef Y.TyInt (Y.VarName "main") [] (body ++ body' ++ newline)]

runToplevelExpr :: (MonadAlpha m, MonadError Error m) => Env -> X.ToplevelExpr -> m [Y.ToplevelStatement]
runToplevelExpr env = \case
  X.ResultExpr e -> do
    t <- typecheckExpr env e
    case t of
      X.FunTy ts ret -> do
        let f = Y.VarName "solve"
        args <- forM ts $ \t -> do
          t <- runType t
          y <- newFreshName ArgumentNameKind ""
          return (t, y)
        ret <- runType ret
        e <- runExpr env e
        let body = [Y.Return (Y.Call (Y.Callable e) (map (Y.Var . snd) args))]
        let solve = [Y.FunDef ret f args body]
        main <- runMain f t
        return $ solve ++ main
      _ -> do
        let x = Y.VarName "ans"
        ans <- runToplevelVarDef env x t e
        main <- runMain x t
        return $ ans ++ main
  X.ToplevelLet x t e cont -> case (e, t) of
    (X.Lam args body, X.FunTy _ ret) -> do
      g <- renameVarName FunctionNameKind x
      stmt <- runToplevelFunDef ((x, t, g) : env) g args ret body
      cont <- runToplevelExpr ((x, t, g) : env) cont
      return $ stmt ++ cont
    _ -> do
      y <- renameVarName ConstantNameKind x
      stmt <- runToplevelVarDef env y t e
      cont <- runToplevelExpr ((x, t, y) : env) cont
      return $ stmt ++ cont
  X.ToplevelLetRec f args ret body cont -> do
    g <- renameVarName FunctionNameKind f
    let t = X.FunTy (map snd args) ret
    stmt <- runToplevelFunDef ((f, t, g) : env) g args ret body
    cont <- runToplevelExpr ((f, t, g) : env) cont
    return $ stmt ++ cont

runProgram :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
runProgram prog = Y.Program <$> runToplevelExpr [] prog

run :: (MonadAlpha m, MonadError Error m) => X.Program -> m Y.Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.FromCore failed" $ do
  runProgram prog
