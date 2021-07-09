-- |
-- Module      : Jikka.RestrictedPython.Convert.RemoveUnreachable
-- Description : removes unreachable statements. / 到達不能な文を削除します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.RemoveUnreachable
  ( run,
  )
where

import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

runStatements :: [Statement] -> [Statement]
runStatements stmts = case break doesAlwaysReturn stmts of
  (stmts, []) -> stmts
  (stmts, stmt : _) -> stmts ++ [stmt]

-- | `run` removes unreachable statements after return-statements.
--
-- For example, the following
--
-- > a = 0
-- > if True:
-- >     b = 0
-- >     return b
-- >     b += 1
-- > else:
-- >     return 1
-- > a += 1
--
-- is converted to
--
-- > a = 0
-- > if True:
-- >     b = 0
-- >     return b
-- > else:
-- >     return 1
run :: Program -> Program
run = mapStatements runStatements
