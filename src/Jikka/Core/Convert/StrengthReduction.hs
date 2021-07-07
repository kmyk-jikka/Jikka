{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Convert.StrengthReduction
-- Description : replace stronger functions in exprs with weaker functions.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Language.Core.StrengthReduction` replaces strong functions in exprs with weaker functions.
-- For example, this replace @abs x@ with @max x (- x)@.
module Jikka.Core.Convert.StrengthReduction
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Lint
import Jikka.Core.Language.RewriteRules
import Jikka.Core.Language.Util
import Jikka.Core.Language.Vars

-- | `eliminateSomeBuiltins` removes some `Builtin` from `Expr` at all.
eliminateSomeBuiltins :: Monad m => RewriteRule m
eliminateSomeBuiltins = simpleRewriteRule $ \case
  -- arithmetical functions
  Minus' e1 e2 -> Just $ Plus' e1 (Negate' e2)
  -- advanced arithmetical functions
  Abs' e -> Just $ Max2' IntTy e (Negate' e)
  Lcm' e1 e2 -> Just $ FloorDiv' (Gcd' e1 e2) (Mult' e1 e2)
  -- logical functions
  Implies' e1 e2 -> Just $ Or' (Not' e1) e2
  -- comparison
  GreaterThan' t e1 e2 -> Just $ LessThan' t e2 e1
  GreaterEqual' t e1 e2 -> Just $ LessEqual' t e2 e1
  NotEqual' t e1 e2 -> Just $ Not' (Equal' t e1 e2)
  _ -> Nothing

-- | `reduceNegate` brings `Negate` to the root.
reduceNegate :: Monad m => RewriteRule m
reduceNegate = simpleRewriteRule $ \case
  Negate' (Negate' e) -> Just e
  Plus' (Negate' e1) (Negate' e2) -> Just $ Negate' (Plus' e1 e2)
  -- `Minus` is already removed.
  Mult' (Negate' e1) e2 -> Just $ Negate' (Mult' e1 e2)
  Mult' e1 (Negate' e2) -> Just $ Negate' (Mult' e1 e2)
  -- `Abs` is already removed.
  Min2' IntTy (Negate' e1) (Negate' e2) -> Just $ Negate' (Max2' IntTy e1 e2)
  Max2' IntTy (Negate' e1) (Negate' e2) -> Just $ Negate' (Min2' IntTy e1 e2)
  _ -> Nothing

-- | `reduceNot` brings `Not` to the root.
reduceNot :: Monad m => RewriteRule m
reduceNot = simpleRewriteRule $ \case
  Not' (Not' e) -> Just e
  And' (Not' e1) (Not' e2) -> Just $ Not' (Or' e1 e2)
  Or' (Not' e1) (Not' e2) -> Just $ Not' (And' e1 e2)
  -- `Implies` is already removed.
  Mult' (Negate' e1) e2 -> Just $ Negate' (Mult' e1 e2)
  Mult' e1 (Negate' e2) -> Just $ Negate' (Mult' e1 e2)
  If' t (Not' e1) e2 e3 -> Just $ If' t e1 e3 e2
  _ -> Nothing

-- | `reduceBitNot` brings `BitNot` to the root.
reduceBitNot :: Monad m => RewriteRule m
reduceBitNot = simpleRewriteRule $ \case
  BitNot' (BitNot' e) -> Just e
  BitAnd' (BitNot' e1) (BitNot' e2) -> Just $ BitNot' (BitOr' e1 e2)
  BitOr' (BitNot' e1) (BitNot' e2) -> Just $ BitNot' (BitAnd' e1 e2)
  BitXor' (BitNot' e1) e2 -> Just $ BitNot' (BitXor' e1 e2)
  BitXor' e1 (BitNot' e2) -> Just $ BitNot' (BitXor' e1 e2)
  _ -> Nothing

reduceAssoc :: Monad m => RewriteRule m
reduceAssoc = simpleRewriteRule $ \case
  Plus' (Plus' e1 e2) e3 -> Just $ Plus' e1 (Plus' e2 e3)
  Minus' (Minus' e1 e2) e3 -> Just $ Minus' e1 (Minus' e2 e3)
  Mult' (Mult' e1 e2) e3 -> Just $ Mult' e1 (Mult' e2 e3)
  Max2' t1 (Max2' t2 e1 e2) e3 -> Just $ Max2' t1 e1 (Max2' t2 e2 e3)
  Min2' t1 (Min2' t2 e1 e2) e3 -> Just $ Min2' t1 e1 (Min2' t2 e2 e3)
  And' (And' e1 e2) e3 -> Just $ And' e1 (And' e2 e3)
  Or' (Or' e1 e2) e3 -> Just $ Or' e1 (Or' e2 e3)
  BitAnd' (BitAnd' e1 e2) e3 -> Just $ BitAnd' e1 (BitAnd' e2 e3)
  BitOr' (BitOr' e1 e2) e3 -> Just $ BitOr' e1 (BitOr' e2 e3)
  BitXor' (BitXor' e1 e2) e3 -> Just $ BitXor' e1 (BitXor' e2 e3)
  _ -> Nothing

-- | `reduceBuild` converts all other list constucting functions to `Range1`.
reduceBuild :: MonadAlpha m => RewriteRule m
reduceBuild = RewriteRule $ \_ -> \case
  Tabulate' t n f -> return . Just $ Map' IntTy t f (Range1' n)
  Range2' l r -> do
    let n = Minus' r l
    x <- genVarName'
    return . Just $ Tabulate' IntTy (Lam x IntTy (Plus' l (Var x))) n
  Range3' l r step -> do
    let n = CeilDiv' (Minus' r l) step
    x <- genVarName'
    return . Just $ Tabulate' IntTy (Lam x IntTy (Plus' l (Mult' step (Var x)))) n
  _ -> return Nothing

reduceMapMap :: MonadAlpha m => RewriteRule m
reduceMapMap = RewriteRule $ \_ -> \case
  -- reduce `Reversed`
  Reversed' _ (Reversed' _ xs) -> return $ Just xs
  Reversed' _ (Map' t1 t2 f xs) -> return . Just $ Map' t1 t2 f (Reversed' t1 xs)
  -- reduce `Sorted`
  Sorted' t (Reversed' _ xs) -> return . Just $ Sorted' t xs
  Sorted' t (Sorted' _ xs) -> return . Just $ Sorted' t xs
  Sorted' _ (Range1' n) -> return . Just $ Range1' n
  Sorted' _ (Map' t1 t2 f xs) -> return . Just $ Map' t1 t2 f (Sorted' t1 xs)
  -- reduce `Map`
  Map' _ _ (LamId _ _) xs -> return $ Just xs
  Map' _ t3 f (Map' t1 _ (Lam x t e) xs) -> return . Just $ Map' t1 t3 (Lam x t (App f e)) xs
  Map' _ t3 g (Map' t1 _ f xs) -> do
    x <- genVarName'
    return . Just $ Map' t1 t3 (Lam x t1 (App g (App f (Var x)))) xs
  _ -> return Nothing

reduceFoldMap :: Monad m => RewriteRule m
reduceFoldMap = simpleRewriteRule $ \case
  -- reduce `Reversed`
  Len' t (Reversed' _ xs) -> Just $ Len' t xs
  At' t (Reversed' _ xs) i -> Just $ At' t xs (Minus' (Minus' (Len' t xs) i) Lit1)
  Sum' (Reversed' _ xs) -> Just $ Sum' xs
  Product' (Reversed' _ xs) -> Just $ Product' xs
  Max1' t (Reversed' _ xs) -> Just $ Max1' t xs
  Min1' t (Reversed' _ xs) -> Just $ Min1' t xs
  ArgMin' t (Reversed' _ xs) -> Just $ ArgMin' t xs
  ArgMax' t (Reversed' _ xs) -> Just $ ArgMax' t xs
  All' (Reversed' _ xs) -> Just $ All' xs
  Any' (Reversed' _ xs) -> Just $ Any' xs
  -- reduce `Sorted`
  Len' t (Sorted' _ xs) -> Just $ Len' t xs
  Sum' (Sorted' _ xs) -> Just $ Sum' xs
  Product' (Sorted' _ xs) -> Just $ Product' xs
  Max1' t (Sorted' _ xs) -> Just $ Max1' t xs
  Min1' t (Sorted' _ xs) -> Just $ Min1' t xs
  All' (Sorted' _ xs) -> Just $ All' xs
  Any' (Sorted' _ xs) -> Just $ Any' xs
  -- reduce `Map`
  Len' _ (Map' t1 _ _ xs) -> Just $ Len' t1 xs
  At' _ (Map' t1 _ f xs) i -> Just $ App f (At' t1 xs i)
  Sum' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Mult' (Len' t1 xs) e
  Sum' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Sum' (Map' t1 t2 (Lam x t e) xs))
  Sum' (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) -> Just $ Plus' (Sum' (Map' t1 t2 (Lam x t e1) xs)) (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Mult' e1 (Sum' (Map' t1 t2 (Lam x t e2) xs))
  Sum' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Mult' e2 (Sum' (Map' t1 t2 (Lam x t e1) xs))
  Product' (Map' t1 _ (Lam x _ e) xs) | x `isUnusedVar` e -> Just $ Pow' e (Len' t1 xs)
  Product' (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Mult' (Pow' (Negate' Lit0) (Len' t1 xs)) (Product' (Map' t1 t2 (Lam x t e) xs))
  Product' (Map' t1 t2 (Lam x t (Mult' e1 e2)) xs) -> Just $ Mult' (Product' (Map' t1 t2 (Lam x t e1) xs)) (Product' (Map' t1 t2 (Lam x t e2) xs))
  Max1' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just e
  Max1' _ (Map' t1 t2 (Lam x t (Max2' t' e1 e2)) xs) -> Just $ Max2' t' (Map' t1 t2 (Lam x t e1) xs) (Map' t1 t2 (Lam x t e2) xs)
  Max1' _ (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Min1' t2 (Map' t1 t2 (Lam x t e) xs))
  Max1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Max1' t2 (Map' t1 t2 (Lam x t e2) xs))
  Max1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Plus' (Max1' t2 (Map' t1 t2 (Lam x t e1) xs)) e1
  Min1' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just e
  Min1' _ (Map' t1 t2 (Lam x t (Min2' t' e1 e2)) xs) -> Just $ Min2' t' (Map' t1 t2 (Lam x t e1) xs) (Map' t1 t2 (Lam x t e2) xs)
  Min1' _ (Map' t1 t2 (Lam x t (Negate' e)) xs) -> Just $ Negate' (Max1' t2 (Map' t1 t2 (Lam x t e) xs))
  Min1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ Plus' e1 (Min1' t2 (Map' t1 t2 (Lam x t e2) xs))
  Min1' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ Plus' (Min1' t2 (Map' t1 t2 (Lam x t e1) xs)) e1
  ArgMax' _ (Map' _ _ (Lam x t e) xs) | x `isUnusedVar` e -> Just $ Minus' (Len' t xs) Lit1
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMax' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMax' t2 (Map' t1 t2 (Lam x t e1) xs)
  ArgMin' _ (Map' _ _ (Lam x _ e) _) | x `isUnusedVar` e -> Just Lit0
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e1 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e2) xs)
  ArgMin' _ (Map' t1 t2 (Lam x t (Plus' e1 e2)) xs) | x `isUnusedVar` e2 -> Just $ ArgMin' t2 (Map' t1 t2 (Lam x t e1) xs)
  _ -> Nothing

reduceFoldBuild :: Monad m => RewriteRule m
reduceFoldBuild = simpleRewriteRule $ \case
  Foldl' _ t (Lam2 x1 t1 x2 _ body) x (Range1' n) | x2 `isUnusedVar` body -> Just $ Iterate' t n (Lam x1 t1 body) x
  Len' _ (Range1' n) -> Just n
  At' _ (Range1' _) i -> Just i
  Sum' (Range1' n) -> Just $ FloorDiv' (Mult' n (Minus' n Lit1)) Lit2
  Sum' (Map' _ _ (Lam x _ (Mult' x' x'')) (Range1' n)) | x' == Var x && x'' == Var x -> Just $ FloorDiv' (Mult' n (Mult' (Minus' n Lit1) (Minus' (Mult' Lit2 n) Lit1))) (Lit (LitInt 6))
  Sum' (Map' _ _ (Lam x _ (Mult' x' (Mult' x'' x'''))) (Range1' n)) | x' == Var x && x'' == Var x && x''' == Var x -> Just $ FloorDiv' (Mult' n (Mult' n (Mult' (Minus' n Lit1) (Minus' n Lit1)))) (Lit (LitInt 4))
  Product' (Range1' n) -> Just $ If' IntTy (Equal' IntTy n Lit0) Lit1 Lit0
  Max1' _ (Range1' n) -> Just $ Minus' n Lit1
  Min1' _ (Range1' _) -> Just Lit0
  ArgMax' _ (Range1' n) -> Just $ Minus' n Lit1
  ArgMin' _ (Range1' _) -> Just Lit0
  _ -> Nothing

reduceFold :: Monad m => RewriteRule m
reduceFold = simpleRewriteRule $ \case
  Iterate' _ k (Lam x _ (MatAp' n _ f (Var x'))) v | x `isUnusedVar` f && x == x' -> Just $ MatAp' n n (MatPow' n f k) v
  _ -> Nothing

reduceList :: MonadAlpha m => RewriteRule m
reduceList =
  mconcat
    [ reduceBuild,
      reduceMapMap,
      reduceFoldMap,
      reduceFoldBuild,
      reduceFold
    ]

misc :: Monad m => RewriteRule m
misc = simpleRewriteRule $ \case
  -- arithmetical functions
  Pow' (Pow' e1 e2) e3 -> Just $ Pow' e1 (Plus' e2 e3)
  -- advanced arithmetical functions
  Gcd' (Mult' k1 e1) (Mult' k2 e2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  Gcd' (Mult' k1 e1) (Mult' e2 k2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  Gcd' (Mult' e1 k1) (Mult' e2 k2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  Gcd' (Mult' e1 k1) (Mult' k2 e2) | k1 == k2 -> Just $ Mult' k1 (Gcd' e1 e2)
  _ -> Nothing

rule :: MonadAlpha m => RewriteRule m
rule =
  mconcat
    [ eliminateSomeBuiltins,
      reduceNegate,
      reduceNot,
      reduceBitNot,
      reduceAssoc,
      reduceList,
      misc
    ]

runProgram :: MonadAlpha m => Program -> m Program
runProgram = applyRewriteRuleProgram' rule

run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.Core.Convert.StrengthReduction" $ do
  precondition $ do
    ensureWellTyped prog
  prog <- runProgram prog
  postcondition $ do
    ensureWellTyped prog
  return prog
