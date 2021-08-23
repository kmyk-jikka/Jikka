{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Jikka.Core.Language.ModuloExpr
  ( -- * Basic functions
    ModuloExpr,
    parseModuloExpr,
    formatTopModuloExpr,
    formatBottomModuloExpr,
    integerModuloExpr,
    negateModuloExpr,
    plusModuloExpr,
    minusModuloExpr,
    multModuloExpr,
    isZeroModuloExpr,
    isOneModuloExpr,
    moduloOfModuloExpr,
    arithmeticExprFromModuloExpr,
  )
where

import Data.List
import Jikka.Common.Error
import Jikka.Core.Format (formatExpr)
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Runtime (modpow)

newtype Modulo = Modulo {unModulo :: Expr}
  deriving (Eq, Ord, Show, Read)

formatModulo :: Modulo -> String
formatModulo = formatExpr . unModulo

data ProductExpr = ProductExpr
  { productExprConst :: Integer,
    productExprList :: [Expr]
  }
  deriving (Eq, Ord, Show, Read)

data SumExpr = SumExpr
  { sumExprList :: [ProductExpr],
    sumExprConst :: Integer
  }
  deriving (Eq, Ord, Show, Read)

data ModuloExpr = ModuloExpr
  { unModuloExpr :: SumExpr,
    modulo :: Modulo
  }
  deriving (Show)

instance Eq ModuloExpr where
  e1 == e2 = unModuloExpr (normalizeModuloExpr e1) == unModuloExpr (normalizeModuloExpr e2) && modulo e1 == modulo e2

instance Ord ModuloExpr where
  e1 `compare` e2 = (unModuloExpr (normalizeModuloExpr e1), modulo e1) `compare` (unModuloExpr (normalizeModuloExpr e2), modulo e2)

negateProductExpr :: ProductExpr -> ProductExpr
negateProductExpr e = e {productExprConst = negate (productExprConst e)}

multProductExpr :: ProductExpr -> ProductExpr -> ProductExpr
multProductExpr e1 e2 =
  ProductExpr
    { productExprConst = productExprConst e1 * productExprConst e2,
      productExprList = productExprList e1 ++ productExprList e2
    }

powProductExpr :: Integer -> ProductExpr -> Integer -> ProductExpr
powProductExpr m e k =
  ProductExpr
    { productExprConst = fromSuccess $ modpow (productExprConst e) k m,
      productExprList = map (\e -> ModPow' e (LitInt' k) (LitInt' m)) (productExprList e)
    }

isInteger :: Modulo -> Bool
isInteger (Modulo (LitInt' _)) = True
isInteger _ = False

moduloToInteger :: Modulo -> Integer
moduloToInteger (Modulo (LitInt' m)) = m
moduloToInteger m = error $ "Jikka.Core.Language.ModuloExpr.moduloToInteger: modulo is not an integer" ++ formatModulo m

parseProductExpr :: Modulo -> Expr -> ProductExpr
parseProductExpr m = \case
  LitInt' n -> ProductExpr {productExprConst = n, productExprList = []}
  Negate' e -> negateProductExpr (parseProductExpr m e)
  Mult' e1 e2 -> multProductExpr (parseProductExpr m e1) (parseProductExpr m e2)
  Pow' e1 (LitInt' k) | isInteger m -> powProductExpr (moduloToInteger m) (parseProductExpr m e1) k
  ModNegate' e m' | Modulo m' == m -> negateProductExpr (parseProductExpr m e)
  ModMult' e1 e2 m' | Modulo m' == m -> multProductExpr (parseProductExpr m e1) (parseProductExpr m e2)
  ModPow' e1 (LitInt' k) (LitInt' m') | Modulo (LitInt' m') == m -> powProductExpr m' (parseProductExpr m e1) k
  e -> ProductExpr {productExprConst = 1, productExprList = [e]}

sumExprFromProductExpr :: ProductExpr -> SumExpr
sumExprFromProductExpr e =
  SumExpr
    { sumExprList = [e],
      sumExprConst = 0
    }

integerSumExpr :: Integer -> SumExpr
integerSumExpr n =
  SumExpr
    { sumExprConst = n,
      sumExprList = []
    }

integerModuloExpr :: Modulo -> Integer -> ModuloExpr
integerModuloExpr m k = ModuloExpr (integerSumExpr k) m

negateSumExpr :: SumExpr -> SumExpr
negateSumExpr e =
  SumExpr
    { sumExprList = map negateProductExpr (sumExprList e),
      sumExprConst = negate (sumExprConst e)
    }

plusSumExpr :: SumExpr -> SumExpr -> SumExpr
plusSumExpr e1 e2 =
  SumExpr
    { sumExprList = sumExprList e1 ++ sumExprList e2,
      sumExprConst = sumExprConst e1 + sumExprConst e2
    }

multSumExpr :: Modulo -> SumExpr -> SumExpr -> SumExpr
multSumExpr m e1 e2 =
  SumExpr
    { sumExprList =
        let es1 = parseProductExpr m (LitInt' (sumExprConst e1)) : sumExprList e1
            es2 = parseProductExpr m (LitInt' (sumExprConst e2)) : sumExprList e2
         in tail $ map (uncurry multProductExpr) ((,) <$> es1 <*> es2),
      sumExprConst = sumExprConst e1 * sumExprConst e2
    }

negateModuloExpr :: ModuloExpr -> ModuloExpr
negateModuloExpr (ModuloExpr e m) = ModuloExpr (negateSumExpr e) m

plusModuloExpr :: ModuloExpr -> ModuloExpr -> Maybe ModuloExpr
plusModuloExpr (ModuloExpr e1 m) (ModuloExpr e2 m') | m == m' = Just $ ModuloExpr (plusSumExpr e1 e2) m
plusModuloExpr _ _ = Nothing

minusModuloExpr :: ModuloExpr -> ModuloExpr -> Maybe ModuloExpr
minusModuloExpr (ModuloExpr e1 m) (ModuloExpr e2 m') | m == m' = Just $ ModuloExpr (plusSumExpr e1 (negateSumExpr e2)) m
minusModuloExpr _ _ = Nothing

multModuloExpr :: ModuloExpr -> ModuloExpr -> Maybe ModuloExpr
multModuloExpr (ModuloExpr e1 m) (ModuloExpr e2 m') | m == m' = Just $ ModuloExpr (multSumExpr m e1 e2) m
multModuloExpr _ _ = Nothing

parseSumExpr :: Modulo -> Expr -> SumExpr
parseSumExpr m = \case
  LitInt' n -> SumExpr {sumExprList = [], sumExprConst = n}
  Negate' e -> negateSumExpr (parseSumExpr m e)
  Plus' e1 e2 -> plusSumExpr (parseSumExpr m e1) (parseSumExpr m e2)
  Minus' e1 e2 -> plusSumExpr (parseSumExpr m e1) (negateSumExpr (parseSumExpr m e2))
  Mult' e1 e2 -> multSumExpr m (parseSumExpr m e1) (parseSumExpr m e2)
  FloorMod' e m' | Modulo m' == m -> parseSumExpr m e
  ModNegate' e m' | Modulo m' == m -> negateSumExpr (parseSumExpr m e)
  ModPlus' e1 e2 m' | Modulo m' == m -> plusSumExpr (parseSumExpr m e1) (parseSumExpr m e2)
  ModMinus' e1 e2 m' | Modulo m' == m -> plusSumExpr (parseSumExpr m e1) (negateSumExpr (parseSumExpr m e2))
  ModMult' e1 e2 m' | Modulo m' == m -> multSumExpr m (parseSumExpr m e1) (parseSumExpr m e2)
  e -> sumExprFromProductExpr (parseProductExpr m e)

getModuloFromExpr :: Expr -> Maybe Modulo
getModuloFromExpr = \case
  FloorMod' _ m -> Just $ Modulo m
  ModNegate' _ m -> Just $ Modulo m
  ModPlus' _ _ m -> Just $ Modulo m
  ModMinus' _ _ m -> Just $ Modulo m
  ModMult' _ _ m -> Just $ Modulo m
  _ -> Nothing

-- | `parseModuloExpr` converts a given expr to a normal form \(\sum_i \prod_j e _ {i,j}) \bmod m\).
-- This assumes given exprs have the type \(\mathbf{int}\).
parseModuloExpr :: Expr -> Maybe ModuloExpr
parseModuloExpr e = do
  m <- getModuloFromExpr e
  return $ ModuloExpr (parseSumExpr m e) m

formatProductExpr :: ProductExpr -> Expr
formatProductExpr e =
  let k = LitInt' (productExprConst e)
      k' e' = case productExprConst e of
        0 -> LitInt' 0
        1 -> e'
        -1 -> Negate' e'
        _ -> Mult' e' k
   in case productExprList e of
        [] -> k
        eHead : esTail -> k' (foldl Mult' eHead esTail)

formatSumExpr :: SumExpr -> Expr
formatSumExpr e = case sumExprList e of
  [] -> LitInt' (sumExprConst e)
  eHead : esTail ->
    let op e'
          | productExprConst e' > 0 = Plus'
          | productExprConst e' < 0 = Minus'
          | otherwise = const
        go e1 e2 = op e2 e1 (formatProductExpr (e2 {productExprConst = abs (productExprConst e2)}))
        k' e'
          | sumExprConst e > 0 = Plus' e' (LitInt' (sumExprConst e))
          | sumExprConst e < 0 = Minus' e' (LitInt' (abs (sumExprConst e)))
          | otherwise = e'
     in k' (foldl go (formatProductExpr eHead) esTail)

-- | `formatTopModuloExpr` convert `ModuloExpr` to `Expr` with adding `FloorMod` at only the root.
formatTopModuloExpr :: ModuloExpr -> Expr
formatTopModuloExpr e = FloorMod' (formatSumExpr . unModuloExpr $ normalizeModuloExpr e) (unModulo $ modulo e)

-- | `formatBottomModuloExpr` convert `ModuloExpr` to `Expr` with adding `FloorMod` at every nodes.
formatBottomModuloExpr :: ModuloExpr -> Expr
formatBottomModuloExpr = error "Jikka.Core.Language.ModuloExpr.formatBottomModuloExpr: TODO: implement this"

normalizeProductExpr :: Modulo -> ProductExpr -> ProductExpr
normalizeProductExpr m e =
  let k = case m of
        Modulo (LitInt' m) -> productExprConst e `mod` m
        _ -> productExprConst e
      es =
        if k == 0
          then []
          else sort (productExprList e)
   in e
        { productExprList = es,
          productExprConst = k
        }

normalizeSumExpr :: Modulo -> SumExpr -> SumExpr
normalizeSumExpr m e =
  let cmp e1 e2 = productExprList e1 `compare` productExprList e2
      cmp' e1 e2 = cmp e1 e2 == EQ
      es = sortBy cmp (map (normalizeProductExpr m) (sumExprList e))
      es' = groupBy cmp' es
      es'' = map (\group -> ProductExpr {productExprConst = sum (map productExprConst group), productExprList = productExprList (head group)}) es'
      es''' = filter (\e -> productExprConst e /= 0 && not (null (productExprList e))) es''
      k = sum (map (\e -> if null (productExprList e) then productExprConst e else 0) es'')
      k' = sumExprConst e + k
      k'' = case m of
        Modulo (LitInt' m) -> k' `mod` m
        _ -> k'
   in SumExpr
        { sumExprList = es''',
          sumExprConst = k''
        }

normalizeModuloExpr :: ModuloExpr -> ModuloExpr
normalizeModuloExpr e = ModuloExpr (normalizeSumExpr (modulo e) (unModuloExpr e)) (modulo e)

isZeroModuloExpr :: ModuloExpr -> Bool
isZeroModuloExpr e = normalizeModuloExpr e == integerModuloExpr (modulo e) 0

isOneModuloExpr :: ModuloExpr -> Bool
isOneModuloExpr e = normalizeModuloExpr e == integerModuloExpr (modulo e) 1

moduloOfModuloExpr :: ModuloExpr -> Expr
moduloOfModuloExpr = unModulo . modulo

arithmeticExprFromModuloExpr :: ModuloExpr -> ArithmeticExpr
arithmeticExprFromModuloExpr = parseArithmeticExpr . formatSumExpr . unModuloExpr
