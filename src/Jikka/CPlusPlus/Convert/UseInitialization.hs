{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.CPlusPlus.Convert.UseInitialization
-- Description : replaces declarations by assignments with initializations. / 代入による宣言を初期化による宣言で置き換えます。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.CPlusPlus.Convert.UseInitialization
  ( run,
  )
where

import Jikka.CPlusPlus.Language.Expr
import Jikka.CPlusPlus.Language.Util
import Jikka.Common.Alpha
import Jikka.Common.Error

runStatement :: Statement -> Statement
runStatement = \case
  Declare t x init -> case (t, init) of
    (TyVector _, DeclareCopy (Call' (VecCtor _) [])) -> Declare t x DeclareDefault
    (TyVector _, DeclareCopy (Call' (VecCtor _) es)) -> Declare t x (DeclareInitialize es)
    (TyConvexHullTrick, DeclareCopy (Call' ConvexHullTrickCtor es)) -> Declare t x (DeclareInitialize es)
    (TySegmentTree _, DeclareCopy (Call' (SegmentTreeCtor _) es)) -> Declare t x (DeclareInitialize es)
    (_, _) -> Declare t x init
  stmt -> stmt

runProgram :: Program -> Program
runProgram = mapExprStatementProgram id ((: []) . runStatement)

-- | `run` unpack tuples.
--
-- == Examples
--
-- Before:
--
-- > vector<int> xs = vector<int>(n, 0);
--
-- After:
--
-- > vector<int> xs(n, 0);
run :: (MonadAlpha m, MonadError Error m) => Program -> m Program
run prog = wrapError' "Jikka.CPlusPlus.Convert.UseInitialization" $ do
  return $ runProgram prog
