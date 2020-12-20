{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.RestrictedPython.Convert.Alpha
  ( run,
  )
where

import qualified Data.Set as S
import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Stdlib

type Env = [(Ident, Ident)]

rename :: MonadAlpha m => Ident -> m Ident
rename x = do
  i <- nextCounter
  let base = if unIdent x == "_" then "" else takeWhile (/= '$') (unIdent x)
  return $ Ident (base ++ '$' : show i)

push :: Ident -> Ident -> Env -> Env
push x y env
  | unIdent x == "_" = env
  | otherwise = (x, y) : env

push' :: [(Ident, Ident, a)] -> Env -> Env
push' xys env = foldl (\env (x, y, _) -> push x y env) env xys

lookup' :: MonadError Error m => Ident -> Env -> m Ident
lookup' x env = case lookup x env of
  Just y -> return y
  Nothing -> throwSymbolError $ "undefined identifier: " ++ unIdent x

runTarget :: (MonadAlpha m, MonadError Error m) => Env -> Target -> m (Target, Env)
runTarget env = \case
  SubscriptTrg t x indices -> do
    x <- lookup' x env
    indices <- mapM (runExpr env) indices
    return (SubscriptTrg t x indices, env)
  NameTrg x -> do
    y <- rename x
    return (NameTrg y, push x y env)
  TupleTrg xs -> do
    xs <- forM xs $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let xs' = map (\(_, y, t) -> (y, t)) xs
    let env' = map (\(x, y, _) -> (x, y)) xs
    return (TupleTrg xs', env')

runExpr :: (MonadAlpha m, MonadError Error m) => Env -> Expr -> m Expr
runExpr env = \case
  BoolOp e1 op e2 -> BoolOp <$> runExpr env e1 <*> return op <*> runExpr env e2
  BinOp e1 op e2 -> BinOp <$> runExpr env e1 <*> return op <*> runExpr env e2
  UnaryOp op e -> UnaryOp op <$> runExpr env e
  Lambda args ret body -> do
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let env' = push' args env
    let args' = map (\(_, y, t) -> (y, t)) args
    body <- runExpr env' body
    return $ Lambda args' ret body
  IfExp t e1 e2 e3 -> do
    e1 <- runExpr env e1
    e2 <- runExpr env e2
    e3 <- runExpr env e3
    return $ IfExp t e1 e2 e3
  ListComp t e (Comprehension x t' iter ifs) -> do
    iter <- runExpr env iter
    (y, env) <- runTarget env x
    ifs <- mapM (runExpr env) ifs
    e <- runExpr env e
    return $ ListComp t e (Comprehension y t' iter ifs)
  Compare e1 op e2 -> Compare <$> runExpr env e1 <*> return op <*> runExpr env e2
  Call t f args -> do
    f <- runExpr env f
    args <- mapM (runExpr env) args
    return $ Call t f args
  Constant const -> return $ Constant const
  Subscript t e1 e2 -> Subscript t <$> runExpr env e1 <*> runExpr env e2
  Name x -> Name <$> lookup' x env
  List t es -> List t <$> mapM (runExpr env) es
  Tuple es -> do
    es <- forM es $ \(e, t) -> do
      e <- runExpr env e
      return (e, t)
    return $ Tuple es
  SubscriptSlice t e from to step -> do
    e <- runExpr env e
    from <- mapM (runExpr env) from
    to <- mapM (runExpr env) to
    step <- mapM (runExpr env) step
    return $ SubscriptSlice t e from to step

runStatement :: (MonadAlpha m, MonadError Error m) => Env -> Statement -> m (Statement, Env)
runStatement env = \case
  Return e -> do
    e <- runExpr env e
    return (Return e, env)
  AugAssign x op e -> do
    e <- runExpr env e
    (x, env) <- runTarget env x
    return (AugAssign x op e, env)
  AnnAssign x t e -> do
    e <- runExpr env e
    (x, env) <- runTarget env x
    return (AnnAssign x t e, env)
  For x t e body -> do
    (y, env) <- runTarget env x
    e <- runExpr env e
    (body, _) <- runStatements env body
    return (For y t e body, env)
  If e body1 body2 -> do
    e <- runExpr env e
    (body1, _) <- runStatements env body1
    (body2, _) <- runStatements env body2
    return (If e body1 body2, env)
  Assert e -> do
    e <- runExpr env e
    return (Assert e, env)

mapM' :: (MonadAlpha m, MonadError Error m) => (Env -> a -> m (b, Env)) -> Env -> [a] -> m ([b], Env)
mapM' f env = \case
  [] -> return ([], env)
  (x : xs) -> do
    y <- catchError' $ f env x
    case y of
      Left err -> do
        ys <- catchError' $ mapM' f env xs
        case ys of
          Left err' -> throwError (err <> err')
          Right _ -> throwError err
      Right (y, env) -> do
        (ys, env) <- mapM' f env xs
        return (y : ys, env)

runStatements :: (MonadAlpha m, MonadError Error m) => Env -> [Statement] -> m ([Statement], Env)
runStatements = mapM' runStatement

runToplevelStatement :: (MonadAlpha m, MonadError Error m) => Env -> ToplevelStatement -> m (ToplevelStatement, Env)
runToplevelStatement env = \case
  ToplevelAnnAssign x t e -> do
    y <- rename x
    let env' = push x y env
    e <- runExpr env' e
    return (ToplevelAnnAssign y t e, env')
  ToplevelFunctionDef f args body ret -> do
    g <- rename f
    let env' = push f g env
    args <- forM args $ \(x, t) -> do
      y <- rename x
      return (x, y, t)
    let args' = map (\(_, y, t) -> (y, t)) args
    let env'' = push' args env'
    (body, _) <- runStatements env'' body
    return (ToplevelFunctionDef g args' body ret, env')
  ToplevelAssert e -> do
    e <- runExpr env e
    return (ToplevelAssert e, env)

runProgram :: (MonadAlpha m, MonadError Error m) => Env -> Program -> m (Program, Env)
runProgram = mapM' runToplevelStatement

initialEnv :: Env
initialEnv = map (\x -> (x, x)) (S.toList builtinFunctions)

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.Alpha" $ do
  (prog, _) <- runProgram initialEnv prog
  return prog
