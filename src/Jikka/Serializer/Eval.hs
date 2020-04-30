module Jikka.Serializer.Eval (run, run') where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Jikka.Language.Type

v :: (a -> Literal) -> a -> Either String Expr
v f x = Right (Lit (f x))

app :: M.Map Name Expr -> Expr -> Either String Expr
app env e = case e of
  App (BuiltIn a) (Lit e1) -> case (a, e1) of
    (Not, Bool p) -> v Bool $ not p
    (Neg, Int n) -> v Int $ negate n
    (Succ, Int n) -> v Int $ succ n
    (Pred, Int n) -> v Int $ pred n
    _ -> return e
  App (App (BuiltIn a) (Lit e1)) (Lit e2) -> case (a, e1, e2) of
    (And, Bool p, Bool q) -> v Bool $ p && q
    (Or, Bool p, Bool q) -> v Bool $ p || q
    (Add, Int m, Int n) -> v Int $ m + n
    (Sub, Int m, Int n) -> v Int $ m - n
    (Mul, Int m, Int n) -> v Int $ m * n
    (Div, Int m, Int n) -> if n == 0 then Left "Runtime Error: divide by zero" else v Int $ m `div` n
    (Mod, Int m, Int n) -> if n == 0 then Left "Runtime Error: modulo by zero" else v Int $ m `mod` n
    (Pow, Int m, Int n) -> v Int $ m ^ n
    (Min, Int m, Int n) -> v Int $ min m n
    (Max, Int m, Int n) -> v Int $ max m n
    (Equal, Int m, Int n) -> v Bool $ m == n
    (NotEqual, Int m, Int n) -> v Bool $ m /= n
    (LessThan, Int m, Int n) -> v Bool $ m < n
    (LessThanEqual, Int m, Int n) -> v Bool $ m <= n
    (GreaterThan, Int m, Int n) -> v Bool $ m > n
    (GreaterThanEqual, Int m, Int n) -> v Bool $ m >= n
    _ -> return e
  App (App (App (BuiltIn a) (Lit e1)) (Lit e2)) (Lit e3) -> case (a, e1, e2, e3) of
    (If, Bool p, _, _) -> Right . Lit $ if p then e2 else e3
    _ -> return e
  App (Fun ftype branches) e1 -> call env ftype branches [e1]
  App (App (Fun ftype branches) e1) e2 -> call env ftype branches [e1, e2]
  App (App (App (Fun ftype branches) e1) e2) e3 -> call env ftype branches [e1, e2, e3]
  App (App (App (App (Fun ftype branches) e1) e2) e3) e4 -> call env ftype branches [e1, e2, e3, e4]
  App (App (App (App (App (Fun ftype branches) e1) e2) e3) e4) e5 -> call env ftype branches [e1, e2, e3, e4, e5]
  _ -> return e

call :: M.Map Name Expr -> FunType -> [([Pattern], Expr)] -> [Expr] -> Either String Expr
call env ftype branches args = head . catMaybes $ candidates ++ [Just $ Right fallback]
  where
    env' = case ftype of
      NoRec -> env
      Rec x -> M.insert x (Fun ftype branches) env
    candidates = map (\branch -> call1 env' branch args) branches
    fallback = foldl App (Fun ftype branches) args

call1 :: M.Map Name Expr -> ([Pattern], Expr) -> [Expr] -> Maybe (Either String Expr)
call1 env (pats, e) = go env pats
  where
    go :: M.Map Name Expr -> [Pattern] -> [Expr] -> Maybe (Either String Expr)
    go env [] [] = Just $ expr env e
    go env (pat : pats) (arg : args) = case (pat, arg) of
      (PatVar x, _) -> go (M.insert x arg env) pats args
      (PatLit Unit, Lit Unit) -> go env pats args
      (PatLit (Int n), Lit (Int n')) | n == n' -> go env pats args
      (PatLit (Bool p), Lit (Bool p')) | p == p' -> go env pats args
      (PatPlusK x k, Lit (Int n)) -> go (M.insert x (Lit . Int $ n - k) env) pats args
      _ -> Nothing
    go _ _ _ = Nothing

expr :: M.Map Name Expr -> Expr -> Either String Expr
expr env e = case e of
  Lit _ -> return e
  Var x -> case M.lookup x env of
    Nothing -> Left $ "Runtime Error: undefined variable " ++ x
    Just e -> return e
  BuiltIn _ -> return e
  Let x t e1 e2 -> do
    e1 <- expr env e1
    expr (M.insert x e1 env) e2
  Fun _ _ -> return e
  App e1 e2 -> do
    e1 <- expr env e1
    e2 <- expr env e2
    app env (App e1 e2)

run' :: M.Map Name Expr -> Expr -> Either String Expr
run' = expr

run :: Program -> Either String Text
run Program {given = [], body = e} = pack . show <$> run' M.empty e
run _ = Left "Runtime Error: we cannot execute programs with given clauses now"
