{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Jikka.Core.Language.BuiltinPatterns
-- Description : provides pattern synonyms for builtin functions. / 組み込み関数のための pattern synonyms を提供します。
-- Copyright   : (c) Kimiyuki Onaka, 2020
-- License     : Apache License 2.0
-- Maintainer  : kimiyuki95@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- `Jikka.Core.Language.BuiltinPatterns` provides pattern synonyms for applications of `Builtin` functions.
-- For example, provide a pattern @Sum' e@ which is interpreted as @AppBuiltin1 Sum [e]@, or the same thing, @App (Lit (LitBuiltin Sum)) [e]@.
module Jikka.Core.Language.BuiltinPatterns where

import Jikka.Core.Language.Expr

-- arithmetical functions
pattern Negate' e = AppBuiltin1 Negate e

pattern Plus' e1 e2 = AppBuiltin2 Plus e1 e2

pattern Minus' e1 e2 = AppBuiltin2 Minus e1 e2

pattern Mult' e1 e2 = AppBuiltin2 Mult e1 e2

pattern FloorDiv' e1 e2 = AppBuiltin2 FloorDiv e1 e2

pattern FloorMod' e1 e2 = AppBuiltin2 FloorMod e1 e2

pattern CeilDiv' e1 e2 = AppBuiltin2 CeilDiv e1 e2

pattern CeilMod' e1 e2 = AppBuiltin2 CeilMod e1 e2

pattern JustDiv' e1 e2 = AppBuiltin2 JustDiv e1 e2

pattern Pow' e1 e2 = AppBuiltin2 Pow e1 e2

-- advanced arithmetical functions
pattern Abs' e = AppBuiltin1 Abs e

pattern Gcd' e1 e2 = AppBuiltin2 Gcd e1 e2

pattern Lcm' e1 e2 = AppBuiltin2 Lcm e1 e2

pattern Min2' t e1 e2 = AppBuiltin12 Min2 t e1 e2

pattern Max2' t e1 e2 = AppBuiltin12 Max2 t e1 e2

pattern Iterate' t n step base = AppBuiltin13 Iterate t n step base

-- logical functions
pattern Not' e = AppBuiltin1 Not e

pattern And' e1 e2 = AppBuiltin2 And e1 e2

pattern Or' e1 e2 = AppBuiltin2 Or e1 e2

pattern Implies' e1 e2 = AppBuiltin2 Implies e1 e2

pattern If' t e1 e2 e3 = AppBuiltin13 If t e1 e2 e3

-- bitwise functions
pattern BitNot' e = AppBuiltin1 BitNot e

pattern BitAnd' e1 e2 = AppBuiltin2 BitAnd e1 e2

pattern BitOr' e1 e2 = AppBuiltin2 BitOr e1 e2

pattern BitXor' e1 e2 = AppBuiltin2 BitXor e1 e2

pattern BitLeftShift' e1 e2 = AppBuiltin2 BitLeftShift e1 e2

pattern BitRightShift' e1 e2 = AppBuiltin2 BitRightShift e1 e2

-- matrix functions

pattern MatAp' h w e1 e2 = AppBuiltin2 (MatAp h w) e1 e2

pattern MatAdd' h w e1 e2 = AppBuiltin2 (MatAdd h w) e1 e2

pattern MatMul' h n w e1 e2 = AppBuiltin2 (MatMul h n w) e1 e2

pattern MatPow' n e1 e2 = AppBuiltin2 (MatPow n) e1 e2

pattern VecFloorMod' n e1 e2 = AppBuiltin2 (VecFloorMod n) e1 e2

pattern MatFloorMod' h w e1 e2 = AppBuiltin2 (MatFloorMod h w) e1 e2

-- modular functions
pattern ModNegate' e1 e2 = AppBuiltin2 ModNegate e1 e2

pattern ModPlus' e1 e2 e3 = AppBuiltin3 ModPlus e1 e2 e3

pattern ModMinus' e1 e2 e3 = AppBuiltin3 ModMinus e1 e2 e3

pattern ModMult' e1 e2 e3 = AppBuiltin3 ModMult e1 e2 e3

pattern ModInv' e1 e2 = AppBuiltin2 ModInv e1 e2

pattern ModPow' e1 e2 e3 = AppBuiltin3 ModPow e1 e2 e3

pattern ModMatAp' h w e1 e2 e3 = AppBuiltin3 (ModMatAp h w) e1 e2 e3

pattern ModMatAdd' h w e1 e2 e3 = AppBuiltin3 (ModMatAdd h w) e1 e2 e3

pattern ModMatMul' h n w e1 e2 e3 = AppBuiltin3 (ModMatMul h n w) e1 e2 e3

pattern ModMatPow' n e1 e2 e3 = AppBuiltin3 (ModMatPow n) e1 e2 e3

-- list functions
pattern Nil' t = Lit (LitNil t)

pattern Cons' t e1 e2 = AppBuiltin12 Cons t e1 e2

pattern Snoc' t e1 e2 = AppBuiltin12 Snoc t e1 e2

pattern Foldl' t1 t2 e1 e2 e3 = AppBuiltin23 Foldl t1 t2 e1 e2 e3

pattern Scanl' t1 t2 e1 e2 e3 = AppBuiltin23 Scanl t1 t2 e1 e2 e3

pattern Build' t e1 e2 e3 = AppBuiltin13 Build t e1 e2 e3

pattern Len' t e = AppBuiltin11 Len t e

pattern Map' t1 t2 f e = AppBuiltin22 Map t1 t2 f e

pattern Filter' t f e = AppBuiltin12 Filter t f e

pattern At' t e1 e2 = AppBuiltin12 At t e1 e2

pattern SetAt' t e1 e2 e3 = AppBuiltin13 SetAt t e1 e2 e3

pattern Elem' t e1 e2 = AppBuiltin12 Elem t e1 e2

pattern Sum' e = AppBuiltin1 Sum e

pattern Product' e = AppBuiltin1 Product e

pattern ModSum' e1 e2 = AppBuiltin2 ModSum e1 e2

pattern ModProduct' e1 e2 = AppBuiltin2 ModProduct e1 e2

pattern Min1' t e = AppBuiltin11 Min1 t e

pattern Max1' t e = AppBuiltin11 Max1 t e

pattern ArgMin' t e = AppBuiltin11 ArgMin t e

pattern ArgMax' t e = AppBuiltin11 ArgMax t e

pattern Gcd1' t e = AppBuiltin11 Gcd1 t e

pattern Lcm1' t e = AppBuiltin11 Lcm1 t e

pattern All' e = AppBuiltin1 All e

pattern Any' e = AppBuiltin1 Any e

pattern Sorted' t e = AppBuiltin11 Sorted t e

pattern Reversed' t e = AppBuiltin11 Reversed t e

pattern Range1' e = AppBuiltin1 Range1 e

pattern Range2' e1 e2 = AppBuiltin2 Range2 e1 e2

pattern Range3' e1 e2 e3 = AppBuiltin3 Range3 e1 e2 e3

-- tuple functions
pattern Tuple' ts = Lit (LitBuiltin Tuple ts)

pattern Proj' ts n e = App (Lit (LitBuiltin (Proj n) ts)) e

-- arithmetical relations
pattern LessThan' t e1 e2 = AppBuiltin12 LessThan t e1 e2

pattern LessEqual' t e1 e2 = AppBuiltin12 LessEqual t e1 e2

pattern GreaterThan' t e1 e2 = AppBuiltin12 GreaterThan t e1 e2

pattern GreaterEqual' t e1 e2 = AppBuiltin12 GreaterEqual t e1 e2

-- equality relations (polymorphic)
pattern Equal' t e1 e2 = AppBuiltin12 Equal t e1 e2

pattern NotEqual' t e1 e2 = AppBuiltin12 NotEqual t e1 e2

-- combinational functions
pattern Fact' e = AppBuiltin1 Fact e

pattern Choose' e1 e2 = AppBuiltin2 Choose e1 e2

pattern Permute' e1 e2 = AppBuiltin2 Permute e1 e2

pattern MultiChoose' e1 e2 = AppBuiltin2 MultiChoose e1 e2

-- data structures
pattern ConvexHullTrickInit' = Builtin ConvexHullTrickInit

pattern ConvexHullTrickGetMin' cht a = AppBuiltin2 ConvexHullTrickGetMin cht a

pattern ConvexHullTrickInsert' cht a b = AppBuiltin3 ConvexHullTrickInsert cht a b

pattern SegmentTreeInitList' semigrp a = AppBuiltin1 (SegmentTreeInitList semigrp) a

pattern SegmentTreeGetRange' semigrp segtree e1 e2 = AppBuiltin3 (SegmentTreeGetRange semigrp) segtree e1 e2

pattern SegmentTreeSetPoint' semigrp segtree e1 e2 = AppBuiltin3 (SegmentTreeSetPoint semigrp) segtree e1 e2

-- errors
pattern Bottom' t err = Lit (LitBottom t err)
