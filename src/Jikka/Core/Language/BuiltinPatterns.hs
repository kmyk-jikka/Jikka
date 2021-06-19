{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Jikka.Core.Language.BuiltinPatterns
-- Description : provides pattern synonyms for `Builtin` applications.
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Core.Language.BuiltinPatterns` provides pattern synonyms for applications of `Builtin` functions.
-- For example, provide a pattern @Sum' e@ which is interpreted as @AppBuiltin Sum [e]@, or the same thing, @App (Lit (LitBuiltin Sum)) [e]@.
module Jikka.Core.Language.BuiltinPatterns where

import Jikka.Core.Language.Expr

-- arithmetical functions
pattern Negate' e = AppBuiltin Negate [e]

pattern Plus' e1 e2 = AppBuiltin Plus [e1, e2]

pattern Minus' e1 e2 = AppBuiltin Minus [e1, e2]

pattern Mult' e1 e2 = AppBuiltin Mult [e1, e2]

pattern FloorDiv' e1 e2 = AppBuiltin FloorDiv [e1, e2]

pattern FloorMod' e1 e2 = AppBuiltin FloorMod [e1, e2]

pattern CeilDiv' e1 e2 = AppBuiltin CeilDiv [e1, e2]

pattern CeilMod' e1 e2 = AppBuiltin CeilMod [e1, e2]

pattern Pow' e1 e2 = AppBuiltin Pow [e1, e2]

-- induction functions
pattern NatInd' t base step n = AppBuiltin (NatInd t) [base, step, n]

-- advanced arithmetical functions
pattern Abs' e = AppBuiltin Abs [e]

pattern Gcd' e1 e2 = AppBuiltin Gcd [e1, e2]

pattern Lcm' e1 e2 = AppBuiltin Lcm [e1, e2]

pattern Min' e1 e2 = AppBuiltin Min [e1, e2]

pattern Max' e1 e2 = AppBuiltin Max [e1, e2]

-- logical functions
pattern Not' e = AppBuiltin Not [e]

pattern And' e1 e2 = AppBuiltin And [e1, e2]

pattern Or' e1 e2 = AppBuiltin Or [e1, e2]

pattern Implies' e1 e2 = AppBuiltin Implies [e1, e2]

pattern If' t e1 e2 e3 = AppBuiltin (If t) [e1, e2, e3]

-- bitwise functions
pattern BitNot' e = AppBuiltin BitNot [e]

pattern BitAnd' e1 e2 = AppBuiltin BitAnd [e1, e2]

pattern BitOr' e1 e2 = AppBuiltin BitOr [e1, e2]

pattern BitXor' e1 e2 = AppBuiltin BitXor [e1, e2]

pattern BitLeftShift' e1 e2 = AppBuiltin BitLeftShift [e1, e2]

pattern BitRightShift' e1 e2 = AppBuiltin BitRightShift [e1, e2]

-- modular functions
pattern Inv' e1 e2 = AppBuiltin Inv [e1, e2]

pattern PowMod' e1 e2 e3 = AppBuiltin PowMod [e1, e2, e3]

-- list functions
pattern Len' t e = AppBuiltin (Len t) [e]

pattern Tabulate' t n f = AppBuiltin (Tabulate t) [n, f]

pattern Map' t1 t2 f e = AppBuiltin (Map t1 t2) [f, e]

pattern At' t e1 e2 = AppBuiltin (At t) [e1, e2]

pattern Sum' e = AppBuiltin Sum [e]

pattern Product' e = AppBuiltin Product [e]

pattern Min1' e = AppBuiltin Min1 [e]

pattern Max1' e = AppBuiltin Max1 [e]

pattern ArgMin' e = AppBuiltin ArgMin [e]

pattern ArgMax' e = AppBuiltin ArgMax [e]

pattern All' e = AppBuiltin All [e]

pattern Any' e = AppBuiltin Any [e]

pattern Sorted' t e = AppBuiltin (Sorted t) [e]

pattern List' t e = AppBuiltin (List t) [e]

pattern Reversed' t e = AppBuiltin (Reversed t) [e]

pattern Range1' e = AppBuiltin Range1 [e]

pattern Range2' e1 e2 = AppBuiltin Range2 [e1, e2]

pattern Range3' e1 e2 e3 = AppBuiltin Range3 [e1, e2, e3]

-- arithmetical relations
pattern LessThan' e1 e2 = AppBuiltin LessThan [e1, e2]

pattern LessEqual' e1 e2 = AppBuiltin LessEqual [e1, e2]

pattern GreaterThan' e1 e2 = AppBuiltin GreaterThan [e1, e2]

pattern GreaterEqual' e1 e2 = AppBuiltin GreaterEqual [e1, e2]

-- equality relations (polymorphic)
pattern Equal' t e1 e2 = AppBuiltin (Equal t) [e1, e2]

pattern NotEqual' t e1 e2 = AppBuiltin (NotEqual t) [e1, e2]

-- combinational functions
pattern Fact' e = AppBuiltin Fact [e]

pattern Choose' e1 e2 = AppBuiltin Choose [e1, e2]

pattern Permute' e1 e2 = AppBuiltin Permute [e1, e2]

pattern MultiChoose' e1 e2 = AppBuiltin MultiChoose [e1, e2]
