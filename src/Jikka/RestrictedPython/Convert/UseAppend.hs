{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.RestrictedPython.Convert.UseAppend
-- Description : converts @xs = xs + [x]@ and @xs += [x]@ to @xs.append(x)@. / @xs = xs + [x]@ と @xs += [x]@ を @xs.append(x)@ に変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.UseAppend
  ( run,
  )
where

import Jikka.Common.Alpha
import Jikka.Common.Error
import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

runStatement :: Statement -> [Statement]
runStatement = \case
  AugAssign xs Add (WithLoc' _ (List t [x])) ->
    [Expr' (withoutLoc (Call (withoutLoc (Attribute (targetToExpr xs) (withoutLoc (BuiltinAppend t)))) [x]))]
  AnnAssign xs t1 (WithLoc' _ (BinOp xs' Add (WithLoc' _ (List t2 [x]))))
    | dropLocation (targetToExpr xs) == dropLocation xs' ->
      let t = case t1 of
            ListTy t -> t
            _ -> t2
       in [Expr' (withoutLoc (Call (withoutLoc (Attribute (targetToExpr xs) (withoutLoc (BuiltinAppend t)))) [x]))]
  stmt -> [stmt]

-- | `run` converts @xs = xs + [x]@ and @xs += [x]@ to @xs.append(x)@.
--
-- == Examples
--
-- Before:
--
-- > xs = xs + [x]
-- > xs += [x]
-- > xs.append(x)
--
-- After:
--
-- > xs.append(x)
-- > xs.append(x)
-- > xs.append(x)
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.RestrictedPython.Convert.UseAppend" $ do
  return $ mapStatement runStatement prog
