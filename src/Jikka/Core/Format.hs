{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jikka.Core.Format
-- Description : converts the syntax trees of core language to strings. / core 言語の構文木を文字列に変換します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- TODO: add parens with considering precedences.
module Jikka.Core.Format
  ( run,
    formatBuiltinIsolated,
    formatBuiltin,
    formatType,
    formatExpr,
    formatProgram,
  )
where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Text (Text, pack)
import Jikka.Common.Format.AutoIndent
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Util

-- | See also Table 2 of <https://www.haskell.org/onlinereport/decls.html Haskell Online Report, 4 Declarations and Bindings>.
newtype Prec = Prec Int
  deriving (Eq, Ord, Show, Read)

instance Enum Prec where
  toEnum n = Prec n
  fromEnum (Prec n) = n

identPrec = Prec 12

funCallPrec = Prec 11

unaryPrec = Prec 10

powerPrec = Prec 8

multPrec = Prec 7

addPrec = Prec 6

appendPrec = Prec 5

comparePrec = Prec 4

andPrec = Prec 3

orPrec = Prec 2

impliesPrec = Prec 1

commaPrec = Prec 0

lambdaPrec = Prec (-1)

parenPrec = Prec (-2)

data Assoc
  = NoAssoc
  | LeftToRight
  | RightToLeft
  deriving (Eq, Ord, Enum, Show, Read)

paren :: String -> String
paren s = "(" ++ s ++ ")"

-- | `resolvePrec` inserts parens to the given string if required.
--
-- >>> resolvePrec multPrec ("1 + 2", addPrec) ++ " * 3"
-- "(1 + 2) * 3"
--
-- >>> resolvePrec addPrec ("1 * 2", multPrec) ++ " + 3"
-- "1 * 2 + 3"
resolvePrec :: Prec -> (String, Prec) -> String
resolvePrec cur (s, prv)
  | cur > prv = paren s
  | otherwise = s

-- | `resolvePrecLeft` inserts parens to the given string if required.
--
-- >>> resolvePrecLeft addPrec LeftToRight ("1 - 2", addPrec) ++ " - 3"
-- "1 - 2 - 3"
resolvePrecLeft :: Prec -> Assoc -> (String, Prec) -> String
resolvePrecLeft cur assoc (s, prv)
  | cur > prv || (cur == prv && assoc /= LeftToRight) = paren s
  | otherwise = s

-- | `resolvePrecRight` inserts parens to the given string if required.
--
-- >>> "1 - " ++ resolvePrecRight addPrec LeftToRight ("2 - 3", addPrec)
-- "1 - (2 - 3)"
resolvePrecRight :: Prec -> Assoc -> (String, Prec) -> String
resolvePrecRight cur assoc (s, prv)
  | cur > prv || (cur == prv && assoc /= RightToLeft) = paren s
  | otherwise = s

formatType' :: Type -> (String, Prec)
formatType' = \case
  VarTy (TypeName a) -> (a, identPrec)
  IntTy -> ("int", identPrec)
  BoolTy -> ("bool", identPrec)
  ListTy t -> (resolvePrec funCallPrec (formatType' t) ++ " list", funCallPrec)
  TupleTy ts -> case ts of
    [t] -> (resolvePrec (pred multPrec) (formatType' t) ++ ",", multPrec)
    _ -> (intercalate " * " (map (resolvePrec (pred multPrec) . formatType') ts), multPrec)
  FunTy t1 t2 ->
    (resolvePrecLeft impliesPrec RightToLeft (formatType' t1) ++ " -> " ++ resolvePrecRight impliesPrec RightToLeft (formatType' t2), impliesPrec)
  DataStructureTy ds -> (formatDataStructure ds, identPrec)

formatType :: Type -> String
formatType = resolvePrec parenPrec . formatType'

formatDataStructure :: DataStructure -> String
formatDataStructure = \case
  ConvexHullTrick -> "convex-hull-trick"
  SegmentTree semigrp -> "segment-tree<" ++ formatSemigroup semigrp ++ ">"

formatSemigroup :: Semigroup' -> String
formatSemigroup = \case
  SemigroupIntPlus -> "int.plus"
  SemigroupIntMin -> "int.min"
  SemigroupIntMax -> "int.max"

data Builtin'
  = Fun [Type] String
  | PrefixOp String
  | InfixOp [Type] String Prec Assoc
  | At' Type
  | If' Type
  deriving (Eq, Ord, Show, Read)

fun :: String -> Builtin'
fun = Fun []

infixOp :: String -> Prec -> Assoc -> Builtin'
infixOp = InfixOp []

analyzeBuiltin :: Builtin -> Builtin'
analyzeBuiltin = \case
  -- arithmetical functions
  Negate -> PrefixOp "-"
  Plus -> infixOp "+" addPrec LeftToRight
  Minus -> infixOp "-" addPrec LeftToRight
  Mult -> infixOp "*" multPrec LeftToRight
  FloorDiv -> infixOp "/" multPrec LeftToRight
  FloorMod -> infixOp "%" multPrec LeftToRight
  CeilDiv -> fun "ceildiv"
  CeilMod -> fun "ceilmod"
  Pow -> infixOp "**" powerPrec RightToLeft
  -- advanced arithmetical functions
  Abs -> fun "abs"
  Gcd -> fun "gcd"
  Lcm -> fun "lcm"
  Min2 t -> Fun [t] "min"
  Max2 t -> Fun [t] "max"
  -- logical functions
  Not -> PrefixOp "not"
  And -> infixOp "and" andPrec RightToLeft
  Or -> infixOp "or" orPrec RightToLeft
  Implies -> infixOp "implies" impliesPrec RightToLeft
  If t -> If' t
  -- bitwise functions
  BitNot -> PrefixOp "~"
  BitAnd -> infixOp "&" multPrec LeftToRight
  BitOr -> infixOp "|" appendPrec LeftToRight
  BitXor -> infixOp "^" addPrec LeftToRight
  BitLeftShift -> infixOp "<<" powerPrec LeftToRight
  BitRightShift -> infixOp ">>" powerPrec LeftToRight
  -- matrix functions
  MatAp _ _ -> fun "matap"
  MatZero _ -> fun "matzero"
  MatOne _ -> fun "matone"
  MatAdd _ _ -> fun "matadd"
  MatMul _ _ _ -> fun "matmul"
  MatPow _ -> fun "matpow"
  VecFloorMod _ -> fun "vecfloormod"
  MatFloorMod _ _ -> fun "matfloormod"
  -- modular functions
  ModNegate -> fun "modnegate"
  ModPlus -> fun "modplus"
  ModMinus -> fun "modminus"
  ModMult -> fun "modmult"
  ModInv -> fun "modinv"
  ModPow -> fun "modpow"
  ModMatAp _ _ -> fun "modmatap"
  ModMatAdd _ _ -> fun "modmatadd"
  ModMatMul _ _ _ -> fun "modmatmul"
  ModMatPow _ -> fun "modmatpow"
  -- list functions
  Cons t -> Fun [t] "cons"
  Snoc t -> Fun [t] "snoc"
  Foldl t1 t2 -> Fun [t1, t2] "foldl"
  Scanl t1 t2 -> Fun [t1, t2] "scanl"
  Build t -> Fun [t] "build"
  Iterate t -> Fun [t] "iterate"
  Len t -> Fun [t] "len"
  Map t1 t2 -> Fun [t1, t2] "map"
  Filter t -> Fun [t] "filter"
  At t -> At' t
  SetAt t -> Fun [t] "setAt"
  Elem t -> Fun [t] "elem"
  Sum -> fun "sum"
  Product -> fun "product"
  ModSum -> fun "modsum"
  ModProduct -> fun "modproduct"
  Min1 t -> Fun [t] "min1"
  Max1 t -> Fun [t] "max1"
  ArgMin t -> Fun [t] "argmin"
  ArgMax t -> Fun [t] "argmax"
  All -> fun "all"
  Any -> fun "any"
  Sorted t -> Fun [t] "sort"
  Reversed t -> Fun [t] "reverse"
  Range1 -> fun "range1"
  Range2 -> fun "range2"
  Range3 -> fun "range3"
  -- tuple functions
  Tuple ts -> Fun ts "tuple"
  Proj ts n -> Fun ts ("proj" ++ show n)
  -- comparison
  LessThan t -> InfixOp [t] "<" comparePrec NoAssoc
  LessEqual t -> InfixOp [t] "<=" comparePrec NoAssoc
  GreaterThan t -> InfixOp [t] ">" comparePrec NoAssoc
  GreaterEqual t -> InfixOp [t] ">=" comparePrec NoAssoc
  Equal t -> InfixOp [t] "==" comparePrec NoAssoc
  NotEqual t -> InfixOp [t] "!=" comparePrec NoAssoc
  -- combinational functions
  Fact -> fun "fact"
  Choose -> fun "choose"
  Permute -> fun "permute"
  MultiChoose -> fun "multichoose"
  -- data structures
  ConvexHullTrickInit -> fun "cht.init"
  ConvexHullTrickGetMin -> fun "cht.getmin"
  ConvexHullTrickInsert -> fun "cht.insert"
  SegmentTreeInitList _ -> fun "segtree.initlist"
  SegmentTreeGetRange _ -> fun "segtree.getrange"
  SegmentTreeSetPoint _ -> fun "segtree.setpoint"

formatTemplate :: [Type] -> String
formatTemplate = \case
  [] -> ""
  ts -> "<" ++ intercalate ", " (map formatType ts) ++ ">"

formatFunCall :: (String, Prec) -> [Expr] -> (String, Prec)
formatFunCall f = \case
  [] -> f
  args -> (resolvePrec funCallPrec f ++ "(" ++ intercalate ", " (map (resolvePrec commaPrec . formatExpr') args) ++ ")", funCallPrec)

formatBuiltinIsolated' :: Builtin' -> String
formatBuiltinIsolated' = \case
  Fun ts name -> name ++ formatTemplate ts
  PrefixOp op -> paren op
  InfixOp ts op _ _ -> paren $ op ++ formatTemplate ts
  At' t -> paren $ "at" ++ formatTemplate [t]
  If' t -> paren $ "if-then-else" ++ formatTemplate [t]

formatBuiltinIsolated :: Builtin -> String
formatBuiltinIsolated = formatBuiltinIsolated' . analyzeBuiltin

formatBuiltin' :: Builtin' -> [Expr] -> (String, Prec)
formatBuiltin' builtin args = case (builtin, args) of
  (Fun _ name, _) -> formatFunCall (name, identPrec) args
  (PrefixOp op, e1 : args) -> formatFunCall (op ++ " " ++ resolvePrec unaryPrec (formatExpr' e1), unaryPrec) args
  (InfixOp _ op prec assoc, e1 : e2 : args) -> formatFunCall (resolvePrecLeft prec assoc (formatExpr' e1) ++ " " ++ op ++ " " ++ resolvePrecRight prec assoc (formatExpr' e2), prec) args
  (At' _, e1 : e2 : args) -> formatFunCall (resolvePrec identPrec (formatExpr' e1) ++ "[" ++ resolvePrec parenPrec (formatExpr' e2) ++ "]", identPrec) args
  (If' _, e1 : e2 : e3 : args) -> formatFunCall ("if" ++ " " ++ resolvePrec parenPrec (formatExpr' e1) ++ " then " ++ resolvePrec parenPrec (formatExpr' e2) ++ " else " ++ resolvePrec lambdaPrec (formatExpr' e3), lambdaPrec) args
  _ -> formatFunCall (formatBuiltinIsolated' builtin, identPrec) args

formatBuiltin :: Builtin -> [Expr] -> String
formatBuiltin f args = resolvePrec parenPrec (formatBuiltin' (analyzeBuiltin f) args)

formatLiteral :: Literal -> String
formatLiteral = \case
  LitBuiltin builtin -> formatBuiltinIsolated builtin
  LitInt n -> show n
  LitBool p -> map toLower $ show p
  LitNil t -> "nil" ++ formatTemplate [t]
  LitBottom t _ -> "bottom" ++ formatTemplate [t]

formatFormalArgs :: [(VarName, Type)] -> String
formatFormalArgs args = unwords $ map (\(x, t) -> paren (unVarName x ++ ": " ++ formatType t)) args

formatExpr' :: Expr -> (String, Prec)
formatExpr' = \case
  Var x -> (unVarName x, identPrec)
  Lit lit -> (formatLiteral lit, identPrec)
  e@(App _ _) ->
    let (f, args) = curryApp e
     in case f of
          Var x -> formatFunCall (unVarName x, identPrec) args
          Lit (LitBuiltin builtin) -> (formatBuiltin builtin args, identPrec)
          _ -> formatFunCall (formatExpr' f) args
  e@(Lam _ _ _) ->
    let (args, body) = uncurryLam e
     in ("fun " ++ formatFormalArgs args ++ " ->\n" ++ indent ++ "\n" ++ resolvePrec parenPrec (formatExpr' body) ++ "\n" ++ dedent ++ "\n", lambdaPrec)
  Let x t e1 e2 -> ("let " ++ unVarName x ++ ": " ++ formatType t ++ " =\n" ++ indent ++ "\n" ++ resolvePrec parenPrec (formatExpr' e1) ++ "\n" ++ dedent ++ "\nin " ++ resolvePrec lambdaPrec (formatExpr' e2), lambdaPrec)

formatExpr :: Expr -> String
formatExpr = unwords . makeIndentFromMarkers 4 . lines . resolvePrec parenPrec . formatExpr'

formatToplevelExpr :: ToplevelExpr -> [String]
formatToplevelExpr = \case
  ResultExpr e -> lines (resolvePrec lambdaPrec (formatExpr' e))
  ToplevelLet x t e cont -> let' (unVarName x) t e cont
  ToplevelLetRec f args ret e cont -> let' ("rec " ++ unVarName f ++ " " ++ formatFormalArgs args) ret e cont
  where
    let' s t e cont =
      ["let " ++ s ++ ": " ++ formatType t ++ " =", indent]
        ++ lines (resolvePrec parenPrec (formatExpr' e))
        ++ [dedent, "in"]
        ++ formatToplevelExpr cont

formatProgram :: Program -> String
formatProgram = unlines . makeIndentFromMarkers 4 . formatToplevelExpr

run :: Applicative m => Program -> m Text
run = pure . pack . formatProgram
