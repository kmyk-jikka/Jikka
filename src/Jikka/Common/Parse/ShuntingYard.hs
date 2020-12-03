{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jikka.Common.Parse.ShuntingYard
  ( run,
    Prec,
    Fixity (..),
    BinOpInfo (..),
  )
where

import Jikka.Common.Error
import Jikka.Common.Language.Pos

type Prec = Int

data Fixity
  = Leftfix
  | Rightfix
  | Nonfix
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data BinOpInfo = BinOpInfo Fixity Prec
  deriving (Eq, Ord, Show, Read)

-- 10.6 Fixity Resolution - Haskell Language Report 2010
-- https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18100010.6
run :: forall m op expr. MonadError Error m => (op -> m BinOpInfo) -> (WithPos op -> WithPos expr -> WithPos expr -> WithPos expr) -> (WithPos expr, [(WithPos op, WithPos expr)]) -> m (WithPos expr)
run info apply (e, tokens) = go [] [e] tokens
  where
    go :: [WithPos op] -> [WithPos expr] -> [(WithPos op, WithPos expr)] -> m (WithPos expr)
    go [] [e1] [] = return e1
    go (op : ops) (e2 : e1 : stk) [] = go ops (apply op e1 e2 : stk) []
    go [] stk ((op, e) : tokens) = go [op] (e : stk) tokens
    go (op1 : ops) (e2 : e1 : stk) ((op2, e3) : tokens) = do
      BinOpInfo fix1 prec1 <- info (value op1)
      BinOpInfo fix2 prec2 <- info (value op2)
      case () of
        -- case (1): check for illegal expressions
        _
          | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) ->
            throwSyntaxErrorAt (pos op1) "illigal expressions due to the fixity of operators"
        -- case (2): op1 and op2 should associate to the left
        _
          | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) ->
            go ops (apply op1 e1 e2 : stk) ((op2, e3) : tokens)
        -- case (3): op1 and op2 should associate to the right
        _
          | otherwise ->
            go (op2 : op1 : ops) (e3 : e2 : e1 : stk) tokens
    go _ _ _ = throwInternalError "failed at shutting-yard algorithm"
