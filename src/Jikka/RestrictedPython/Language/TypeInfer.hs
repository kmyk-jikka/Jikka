{-# LANGUAGE FlexibleContexts #-}

module Jikka.RestrictedPython.Language.TypeInfer
  ( infer',
    infer,
    check',
    check,
  )
where

import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr

infer' :: MonadError Error m => [(Ident, Type)] -> Expr -> m Type
infer' _ _ = undefined

-- | 'infer' extracts the type of the given expr, under the assumption that the expr is correctly typed.
infer :: [(Ident, Type)] -> Expr -> Type
infer env e = case infer' env e of
  Right t -> t
  Left err -> bug $ "failed to infer the type of: " ++ show e ++ ": " ++ show err

check' :: MonadError Error m => [(Ident, Type)] -> Expr -> m Type
check' _ _ = undefined

-- | 'check' calculates the type of the given expr, with strict checking about their Church-style types. This function says nothing about Curry-style types.
check :: MonadError Error m => Expr -> m Type
check e = check' [] e
