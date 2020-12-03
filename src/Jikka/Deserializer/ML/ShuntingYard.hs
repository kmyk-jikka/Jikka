{-# LANGUAGE ScopedTypeVariables #-}

module Jikka.Deserializer.ML.ShuntingYard
  ( run,
    getBuiltInOp,
    getOpName,
    Op,
  )
where

import qualified Data.Map.Strict as M
import Jikka.Deserializer.ML.Pos

type Name = String

type Prec = Int

data Fixity
  = Leftfix
  | Rightfix
  | Nonfix
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Op = Op Fixity Prec Name
  deriving (Eq, Ord, Show, Read)

builtInOps :: M.Map Name Op
builtInOps =
  M.fromList $
    map
      (\op@(Op _ _ name) -> (name, op))
      [ Op Rightfix 8 "**",
        Op Leftfix 7 "*",
        Op Leftfix 7 "/",
        Op Leftfix 7 "%",
        Op Leftfix 6 "+",
        Op Leftfix 6 "-",
        Op Nonfix 4 "==",
        Op Nonfix 4 "/=",
        Op Nonfix 4 "<",
        Op Nonfix 4 "<=",
        Op Nonfix 4 ">",
        Op Nonfix 4 ">=",
        Op Rightfix 3 "&&",
        Op Rightfix 2 "||"
      ]

getBuiltInOp :: WithPos Name -> Either String (WithPos Op)
getBuiltInOp name = case M.lookup (value name) builtInOps of
  Just op -> return $ withPos name op
  Nothing -> error $ "Syntax error at " ++ prettyPos (pos name) ++ ": unkown operator: " ++ value name

getOpName :: Op -> Name
getOpName (Op _ _ name) = name

-- 10.6 Fixity Resolution - Haskell Language Report 2010
-- https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18100010.6
run :: forall expr. (WithPos Op -> WithPos expr -> WithPos expr -> WithPos expr) -> (WithPos expr, [(WithPos Op, WithPos expr)]) -> Either String (WithPos expr)
run app (e, tokens) = go [] [e] tokens
  where
    go :: [WithPos Op] -> [WithPos expr] -> [(WithPos Op, WithPos expr)] -> Either String (WithPos expr)
    go [] [e1] [] = return e1
    go (op : ops) (e2 : e1 : stk) [] = go ops (app op e1 e2 : stk) []
    go [] stk ((op, e) : tokens) = go [op] (e : stk) tokens
    go (op1 : ops) (e2 : e1 : stk) ((op2, e3) : tokens) =
      let (Op fix1 prec1 _) = value op1
       in let (Op fix2 prec2 _) = value op2
           in case () of
                -- case (1): check for illegal expressions
                _
                  | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) ->
                    error $ "Syntax error at " ++ prettyPos (pos op1) ++ ": illigal expressions due to the fixity of operators"
                -- case (2): op1 and op2 should associate to the left
                _
                  | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) ->
                    go ops (app op1 e1 e2 : stk) ((op2, e3) : tokens)
                -- case (3): op1 and op2 should associate to the right
                _
                  | otherwise ->
                    go (op2 : op1 : ops) (e3 : e2 : e1 : stk) tokens
    go _ _ _ = error "Syntax error: a bug of shutting-yard algorithm"
