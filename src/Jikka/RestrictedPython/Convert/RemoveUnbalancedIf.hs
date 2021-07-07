-- |
-- Module      : Jikka.RestrictedPython.Convert.RemoveUnbalancedIf
-- Description : converts and removes if-statements whose either branch has return-statements and the other branch doesn't have return-statements. / その一方の分岐は return 文を持ちもう一方の分岐は return 文を持たないような if 文を変形し削除します。
-- Copyright   : (c) Kimiyuki Onaka, 2021
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
module Jikka.RestrictedPython.Convert.RemoveUnbalancedIf
  ( run,
  )
where

import Jikka.RestrictedPython.Language.Expr
import Jikka.RestrictedPython.Language.Util

runStatements :: [Statement] -> [Statement]
runStatements [] = []
runStatements (stmt : stmts) = case stmt of
  If e body1 body2 -> case (any doesAlwaysReturn body1, any doesAlwaysReturn body2) of
    (True, False) -> [If e body1 (body2 ++ runStatements stmts)]
    (False, True) -> [If e (body1 ++ runStatements stmts) body2]
    _ -> stmt : runStatements stmts
  _ -> stmt : runStatements stmts

-- | `run` removes if-statements that one branch always returns and the other branch doesn't.
--
-- For example, the following
--
-- > if True:
-- >     return 0
-- > else:
-- >     a = 0
-- > b = 1
-- > return 2
--
-- is converted to
--
-- > if True:
-- >     return 0
-- > else:
-- >     a = 0
-- >     b = 1
-- >     return 2
run :: Program -> Program
run = mapStatements runStatements
