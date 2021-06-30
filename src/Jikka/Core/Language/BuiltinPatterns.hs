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

pattern Min2' t e1 e2 = AppBuiltin (Min2 t) [e1, e2]

pattern Max2' t e1 e2 = AppBuiltin (Max2 t) [e1, e2]

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

-- matrix functions

pattern MatAp' h w e1 e2 = AppBuiltin (MatAp h w) [e1, e2]

pattern MatZero' n = AppBuiltin (MatZero n) []

pattern MatOne' n = AppBuiltin (MatOne n) []

pattern MatAdd' h w e1 e2 = AppBuiltin (MatAdd h w) [e1, e2]

pattern MatMul' h n w e1 e2 = AppBuiltin (MatMul h n w) [e1, e2]

pattern MatPow' n e1 e2 = AppBuiltin (MatPow n) [e1, e2]

-- modular functions
pattern ModInv' e1 e2 = AppBuiltin ModInv [e1, e2]

pattern ModPow' e1 e2 e3 = AppBuiltin ModPow [e1, e2, e3]

pattern ModMatAp' h w e1 e2 e3 = AppBuiltin (ModMatAp h w) [e1, e2, e3]

pattern ModMatAdd' h w e1 e2 e3 = AppBuiltin (ModMatAdd h w) [e1, e2, e3]

pattern ModMatMul' h n w e1 e2 e3 = AppBuiltin (ModMatMul h n w) [e1, e2, e3]

pattern ModMatPow' n e1 e2 e3 = AppBuiltin (ModMatPow n) [e1, e2, e3]

-- list functions
pattern Cons' t e1 e2 = AppBuiltin (Cons t) [e1, e2]

pattern Foldl' t1 t2 e1 e2 e3 = AppBuiltin (Foldl t1 t2) [e1, e2, e3]

pattern Scanl' t1 t2 e1 e2 e3 = AppBuiltin (Scanl t1 t2) [e1, e2, e3]

pattern Len' t e = AppBuiltin (Len t) [e]

pattern Tabulate' t n f = AppBuiltin (Tabulate t) [n, f]

pattern Map' t1 t2 f e = AppBuiltin (Map t1 t2) [f, e]

pattern Filter' t f e = AppBuiltin (Filter t) [f, e]

pattern At' t e1 e2 = AppBuiltin (At t) [e1, e2]

pattern SetAt' t e1 e2 e3 = AppBuiltin (SetAt t) [e1, e2, e3]

pattern Elem' t e1 e2 = AppBuiltin (Elem t) [e1, e2]

pattern Sum' e = AppBuiltin Sum [e]

pattern Product' e = AppBuiltin Product [e]

pattern ModProduct' e1 e2 = AppBuiltin ModProduct [e1, e2]

pattern Min1' t e = AppBuiltin (Min1 t) [e]

pattern Max1' t e = AppBuiltin (Max1 t) [e]

pattern ArgMin' t e = AppBuiltin (ArgMin t) [e]

pattern ArgMax' t e = AppBuiltin (ArgMax t) [e]

pattern All' e = AppBuiltin All [e]

pattern Any' e = AppBuiltin Any [e]

pattern Sorted' t e = AppBuiltin (Sorted t) [e]

pattern List' t e = AppBuiltin (List t) [e]

pattern Reversed' t e = AppBuiltin (Reversed t) [e]

pattern Range1' e = AppBuiltin Range1 [e]

pattern Range2' e1 e2 = AppBuiltin Range2 [e1, e2]

pattern Range3' e1 e2 e3 = AppBuiltin Range3 [e1, e2, e3]

-- tuple functions
pattern Tuple' ts es = AppBuiltin (Tuple ts) es

pattern Proj' ts n e = AppBuiltin (Proj ts n) [e]

-- arithmetical relations
pattern LessThan' t e1 e2 = AppBuiltin (LessThan t) [e1, e2]

pattern LessEqual' t e1 e2 = AppBuiltin (LessEqual t) [e1, e2]

pattern GreaterThan' t e1 e2 = AppBuiltin (GreaterThan t) [e1, e2]

pattern GreaterEqual' t e1 e2 = AppBuiltin (GreaterEqual t) [e1, e2]

-- equality relations (polymorphic)
pattern Equal' t e1 e2 = AppBuiltin (Equal t) [e1, e2]

pattern NotEqual' t e1 e2 = AppBuiltin (NotEqual t) [e1, e2]

-- combinational functions
pattern Fact' e = AppBuiltin Fact [e]

pattern Choose' e1 e2 = AppBuiltin Choose [e1, e2]

pattern Permute' e1 e2 = AppBuiltin Permute [e1, e2]

pattern MultiChoose' e1 e2 = AppBuiltin MultiChoose [e1, e2]
