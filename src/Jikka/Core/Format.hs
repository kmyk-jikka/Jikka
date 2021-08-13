{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

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
import Jikka.Core.Language.BuiltinPatterns (pattern Range1')
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars (isUnusedVar)
import Jikka.Core.Language.LambdaPatterns
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
    [] -> ("unit", identPrec)
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
  SemigroupIntGcd -> "int.gcd"
  SemigroupIntLcm -> "int.lcm"

data Builtin'
  = Fun String
  | PrefixOp String
  | InfixOp String Prec Assoc
  | At'
  | SetAt'
  | Tuple'
  | Proj' Integer
  | If'
  deriving (Eq, Ord, Show, Read)

analyzeBuiltin :: Builtin -> Builtin'
analyzeBuiltin = \case
  -- arithmetical functions
  Negate -> PrefixOp "-"
  Plus -> InfixOp "+" addPrec LeftToRight
  Minus -> InfixOp "-" addPrec LeftToRight
  Mult -> InfixOp "*" multPrec LeftToRight
  FloorDiv -> InfixOp "/" multPrec LeftToRight
  FloorMod -> InfixOp "%" multPrec LeftToRight
  CeilDiv -> InfixOp "/^" multPrec LeftToRight
  CeilMod -> InfixOp "%^" multPrec LeftToRight
  Pow -> InfixOp "**" powerPrec RightToLeft
  -- advanced arithmetical functions
  Abs -> Fun "abs"
  Gcd -> Fun "gcd"
  Lcm -> Fun "lcm"
  Min2 -> Fun "min"
  Max2 -> Fun "max"
  -- logical functions
  Not -> Fun "not"
  And -> InfixOp "&&" andPrec RightToLeft
  Or -> InfixOp "||" orPrec RightToLeft
  Implies -> Fun "implies"
  If -> If'
  -- bitwise functions
  BitNot -> PrefixOp "~"
  BitAnd -> InfixOp "&" multPrec LeftToRight
  BitOr -> InfixOp "|" appendPrec LeftToRight
  BitXor -> InfixOp "^" addPrec LeftToRight
  BitLeftShift -> InfixOp "<<" powerPrec LeftToRight
  BitRightShift -> InfixOp ">>" powerPrec LeftToRight
  -- matrix functions
  MatAp _ _ -> Fun "matap"
  MatZero _ -> Fun "matzero"
  MatOne _ -> Fun "matone"
  MatAdd _ _ -> Fun "matadd"
  MatMul _ _ _ -> Fun "matmul"
  MatPow _ -> Fun "matpow"
  VecFloorMod _ -> Fun "vecfloormod"
  MatFloorMod _ _ -> Fun "matfloormod"
  -- modular functions
  ModNegate -> Fun "modnegate"
  ModPlus -> Fun "modplus"
  ModMinus -> Fun "modminus"
  ModMult -> Fun "modmult"
  ModInv -> Fun "modinv"
  ModPow -> Fun "modpow"
  ModMatAp _ _ -> Fun "modmatap"
  ModMatAdd _ _ -> Fun "modmatadd"
  ModMatMul _ _ _ -> Fun "modmatmul"
  ModMatPow _ -> Fun "modmatpow"
  -- list functions
  Cons -> Fun "cons"
  Snoc -> Fun "snoc"
  Foldl -> Fun "foldl"
  Scanl -> Fun "scanl"
  Build -> Fun "build"
  Iterate -> Fun "iterate"
  Len -> Fun "len"
  Map -> Fun "map"
  Filter -> Fun "filter"
  At -> At'
  SetAt -> SetAt'
  Elem -> Fun "elem"
  Sum -> Fun "sum"
  Product -> Fun "product"
  ModSum -> Fun "modsum"
  ModProduct -> Fun "modproduct"
  Min1 -> Fun "minimum"
  Max1 -> Fun "maximum"
  ArgMin -> Fun "argmin"
  ArgMax -> Fun "argmax"
  Gcd1 -> Fun "gcds"
  Lcm1 -> Fun "lcms"
  All -> Fun "all"
  Any -> Fun "any"
  Sorted -> Fun "sort"
  Reversed -> Fun "reverse"
  Range1 -> Fun "range"
  Range2 -> Fun "range2"
  Range3 -> Fun "range3"
  -- tuple functions
  Tuple -> Tuple'
  Proj n -> Proj' n
  -- comparison
  LessThan -> InfixOp "<" comparePrec NoAssoc
  LessEqual -> InfixOp "<=" comparePrec NoAssoc
  GreaterThan -> InfixOp ">" comparePrec NoAssoc
  GreaterEqual -> InfixOp ">=" comparePrec NoAssoc
  Equal -> InfixOp "==" comparePrec NoAssoc
  NotEqual -> InfixOp "!=" comparePrec NoAssoc
  -- combinational functions
  Fact -> Fun "fact"
  Choose -> Fun "choose"
  Permute -> Fun "permute"
  MultiChoose -> Fun "multichoose"
  -- data structures
  ConvexHullTrickInit -> Fun "cht.init"
  ConvexHullTrickGetMin -> Fun "cht.getmin"
  ConvexHullTrickInsert -> Fun "cht.insert"
  SegmentTreeInitList _ -> Fun "segtree.initlist"
  SegmentTreeGetRange _ -> Fun "segtree.getrange"
  SegmentTreeSetPoint _ -> Fun "segtree.setpoint"

formatTemplate :: [Type] -> String
formatTemplate = \case
  [] -> ""
  ts -> "<" ++ intercalate ", " (map formatType ts) ++ ">"

formatFunCall :: (String, Prec) -> [Expr] -> (String, Prec)
formatFunCall f [] = f
formatFunCall f args =
  let f' = resolvePrecLeft funCallPrec LeftToRight f
      args' = map (resolvePrecRight funCallPrec LeftToRight . formatExpr') args
   in (unwords (f' : args'), funCallPrec)

formatBuiltinIsolated' :: Builtin' -> [Type] -> String
formatBuiltinIsolated' builtin ts = case builtin of
  Fun name -> name ++ formatTemplate ts
  PrefixOp op -> paren $ op ++ formatTemplate ts
  InfixOp op _ _ -> paren $ op ++ formatTemplate ts
  At' -> paren $ "at" ++ formatTemplate ts
  SetAt' -> paren $ "set-at" ++ formatTemplate ts
  Tuple' -> paren $ "tuple" ++ formatTemplate ts
  Proj' n -> paren $ "proj-" ++ show n ++ formatTemplate ts
  If' -> paren $ "if-then-else" ++ formatTemplate ts

formatBuiltinIsolated :: Builtin -> [Type] -> String
formatBuiltinIsolated builtin ts = formatBuiltinIsolated' (analyzeBuiltin builtin) ts

formatBuiltin' :: Builtin -> [Type] -> [Expr] -> (String, Prec)
formatBuiltin' builtin ts args = case (analyzeBuiltin builtin, ts, args) of
  (Fun "map", _, [Lam x IntTy e, Range1' n]) | x `isUnusedVar` e -> formatFunCall ("replicate", identPrec) [n, e]
  (Fun name, _, _) -> formatFunCall (name, identPrec) args
  (PrefixOp op, _, e1 : args) -> formatFunCall (op ++ " " ++ resolvePrec unaryPrec (formatExpr' e1), unaryPrec) args
  (InfixOp op prec assoc, _, e1 : e2 : args) -> formatFunCall (resolvePrecLeft prec assoc (formatExpr' e1) ++ " " ++ op ++ " " ++ resolvePrecRight prec assoc (formatExpr' e2), prec) args
  (At', _, e1 : e2 : args) -> formatFunCall (resolvePrec identPrec (formatExpr' e1) ++ "[" ++ resolvePrec parenPrec (formatExpr' e2) ++ "]", identPrec) args
  (SetAt', _, e1 : e2 : e3 : args) -> formatFunCall (resolvePrec identPrec (formatExpr' e1) ++ "[" ++ resolvePrec parenPrec (formatExpr' e2) ++ " <- " ++ resolvePrec parenPrec (formatExpr' e3) ++ "]", identPrec) args
  (Tuple', [_], e : args) -> formatFunCall (paren (resolvePrec commaPrec (formatExpr' e) ++ ","), identPrec) args
  (Tuple', _, args) | length args >= length ts -> formatFunCall (paren (intercalate ", " (map (resolvePrec commaPrec . formatExpr') (take (length ts) args))), identPrec) (drop (length ts) args)
  (Proj' n, _, e : args) -> formatFunCall (resolvePrec identPrec (formatExpr' e) ++ "." ++ show n, identPrec) args
  (If', _, e1 : e2 : e3 : args) -> formatFunCall ("if" ++ " " ++ resolvePrec parenPrec (formatExpr' e1) ++ " then " ++ resolvePrec parenPrec (formatExpr' e2) ++ " else " ++ resolvePrec lambdaPrec (formatExpr' e3), lambdaPrec) args
  _ -> formatFunCall (formatBuiltinIsolated' (analyzeBuiltin builtin) ts, identPrec) args

formatBuiltin :: Builtin -> [Type] -> [Expr] -> String
formatBuiltin f ts args = resolvePrec parenPrec (formatBuiltin' f ts args)

formatLiteral :: Literal -> String
formatLiteral = \case
  LitBuiltin builtin ts -> formatBuiltinIsolated builtin ts
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
          Lit (LitBuiltin builtin ts) -> formatBuiltin' builtin ts args
          _ -> formatFunCall (formatExpr' f) args
  LamId _ -> ("id", identPrec)
  LamConst _ e -> formatFunCall ("const", identPrec) [e]
  e@(Lam _ _ _) ->
    let (args, body) = uncurryLam e
     in ("fun " ++ formatFormalArgs args ++ " ->\n" ++ indent ++ "\n" ++ resolvePrec parenPrec (formatExpr' body) ++ "\n" ++ dedent ++ "\n", lambdaPrec)
  Let x t e1 e2 -> ("let " ++ unVarName x ++ ": " ++ formatType t ++ " =\n" ++ indent ++ "\n" ++ resolvePrec parenPrec (formatExpr' e1) ++ "\n" ++ dedent ++ "\nin " ++ resolvePrec lambdaPrec (formatExpr' e2), lambdaPrec)
  Assert e1 e2 -> ("assert " ++ resolvePrec parenPrec (formatExpr' e1) ++ " in " ++ resolvePrec lambdaPrec (formatExpr' e2), lambdaPrec)

formatExpr :: Expr -> String
formatExpr = unlines . makeIndentFromMarkers 4 . lines . resolvePrec parenPrec . formatExpr'

formatToplevelExpr :: ToplevelExpr -> [String]
formatToplevelExpr = \case
  ResultExpr e -> lines (resolvePrec lambdaPrec (formatExpr' e))
  ToplevelLet x t e cont -> let' (unVarName x) t e cont
  ToplevelLetRec f args ret e cont -> let' ("rec " ++ unVarName f ++ " " ++ formatFormalArgs args) ret e cont
  ToplevelAssert e cont -> ["assert " ++ resolvePrec parenPrec (formatExpr' e), "in"] ++ formatToplevelExpr cont
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
