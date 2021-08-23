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

    -- * Utilities
    Modulo (..),
    formatModulo,
    isModulo,
  )
where

import Data.List
import Jikka.Common.Error
import Jikka.Core.Format (formatExpr)
import Jikka.Core.Language.ArithmeticExpr
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.Runtime (modinv, modpow)
import Jikka.Core.Language.Util

-- | `Modulo` is just a newtype to avoid mistakes that swapping left and right of mod-op.
newtype Modulo = Modulo {unModulo :: Expr}
  deriving (Eq, Ord, Show, Read)

formatModulo :: Modulo -> String
formatModulo = formatExpr . unModulo

isModulo :: Expr -> Modulo -> Bool
isModulo e (Modulo m) = case e of
  FloorMod' _ m' -> m' == m
  ModNegate' _ m' -> m' == m
  ModPlus' _ _ m' -> m' == m
  ModMinus' _ _ m' -> m' == m
  ModMult' _ _ m' -> m' == m
  ModInv' _ m' -> m' == m
  ModPow' _ _ m' -> m' == m
  VecFloorMod' _ _ m' -> m' == m
  MatFloorMod' _ _ _ m' -> m' == m
  ModMatAp' _ _ _ _ m' -> m' == m
  ModMatAdd' _ _ _ _ m' -> m' == m
  ModMatMul' _ _ _ _ _ m' -> m' == m
  ModMatPow' _ _ _ m' -> m' == m
  ModSum' _ m' -> m' == m
  ModProduct' _ m' -> m' == m
  LitInt' n -> case m of
    LitInt' m -> 0 <= n && n < m
    _ -> False
  Proj' ts _ e | isVectorTy' ts -> e `isModulo` Modulo m
  Proj' ts _ e | isMatrixTy' ts -> e `isModulo` Modulo m
  Map' _ _ f _ -> f `isModulo` Modulo m
  Lam _ _ body -> body `isModulo` Modulo m
  e@(App _ _) -> case curryApp e of
    (e@(Lam _ _ _), _) -> e `isModulo` Modulo m
    (Tuple' ts, es) | isVectorTy' ts -> all (`isModulo` Modulo m) es
    (Tuple' ts, es) | isMatrixTy' ts -> all (`isModulo` Modulo m) es
    _ -> False
  _ -> False

data ProductExpr = ProductExpr
  { productExprConst :: Integer,
    productExprList :: [Expr],
    productExprInvList :: [Expr]
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
      productExprList = productExprList e1 ++ productExprList e2,
      productExprInvList = productExprInvList e1 ++ productExprInvList e2
    }

powProductExpr :: Integer -> ProductExpr -> Integer -> ProductExpr
powProductExpr m e k =
  ProductExpr
    { productExprConst = fromSuccess $ modpow (productExprConst e) k m,
      productExprList = map (\e -> ModPow' e (LitInt' k) (LitInt' m)) (productExprList e),
      productExprInvList = map (\e -> ModPow' e (LitInt' k) (LitInt' m)) (productExprInvList e)
    }

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f = \case
  [] -> ([], [])
  x : xs ->
    let (ys, xs') = partitionMaybe f xs
     in case f x of
          Just y -> (y : ys, xs')
          Nothing -> (ys, x : xs')

fromLitInt :: Expr -> Maybe Integer
fromLitInt (LitInt' k) = Just k
fromLitInt _ = Nothing

invProductExpr :: Modulo -> ProductExpr -> ProductExpr
invProductExpr m e =
  let (invKs, invList) = partitionMaybe fromLitInt (productExprInvList e)
      e' =
        ProductExpr
          { productExprConst = product invKs,
            productExprList = LitInt' (productExprConst e) : invList,
            productExprInvList = productExprList e
          }
   in case m of
        Modulo (LitInt' m) -> case modinv (productExprConst e) m of
          Right k ->
            ProductExpr
              { productExprConst = (k * product invKs) `mod` m,
                productExprList = invList,
                productExprInvList = productExprList e
              }
          _ -> e'
        _ -> e'

isInteger :: Modulo -> Bool
isInteger (Modulo (LitInt' _)) = True
isInteger _ = False

moduloToInteger :: Modulo -> Integer
moduloToInteger (Modulo (LitInt' m)) = m
moduloToInteger m = error $ "Jikka.Core.Language.ModuloExpr.moduloToInteger: modulo is not an integer" ++ formatModulo m

parseProductExpr :: Modulo -> Expr -> ProductExpr
parseProductExpr m = \case
  LitInt' n -> ProductExpr {productExprConst = n, productExprList = [], productExprInvList = []}
  Negate' e -> negateProductExpr (parseProductExpr m e)
  Mult' e1 e2 -> multProductExpr (parseProductExpr m e1) (parseProductExpr m e2)
  JustDiv' e1 e2 -> multProductExpr (parseProductExpr m e1) (invProductExpr m (parseProductExpr m e2))
  Pow' e1 (LitInt' k) | isInteger m -> powProductExpr (moduloToInteger m) (parseProductExpr m e1) k
  ModNegate' e m' | Modulo m' == m -> negateProductExpr (parseProductExpr m e)
  ModMult' e1 e2 m' | Modulo m' == m -> multProductExpr (parseProductExpr m e1) (parseProductExpr m e2)
  ModPow' e1 (LitInt' k) (LitInt' m') | Modulo (LitInt' m') == m -> powProductExpr m' (parseProductExpr m e1) k
  ModInv' e1 m' | Modulo m' == m -> invProductExpr m (parseProductExpr m e1)
  e -> ProductExpr {productExprConst = 1, productExprList = [e], productExprInvList = []}

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

formatTopProductExpr :: Modulo -> ProductExpr -> Expr
formatTopProductExpr m e =
  let k = LitInt' (productExprConst e)
      k' e' = case productExprConst e of
        0 -> LitInt' 0
        1 -> e'
        -1 -> Negate' e'
        _ -> Mult' e' k
      invList = map (`FloorMod'` unModulo m) (productExprInvList e)
   in case productExprList e ++ invList of
        [] -> k
        eHead : esTail -> k' (foldl Mult' eHead esTail)

formatTopSumExpr :: Modulo -> SumExpr -> Expr
formatTopSumExpr m e = case sumExprList e of
  [] -> LitInt' (sumExprConst e)
  eHead : esTail ->
    let op e'
          | productExprConst e' > 0 = Plus'
          | productExprConst e' < 0 = Minus'
          | otherwise = const
        go e1 e2 = op e2 e1 (formatTopProductExpr m (e2 {productExprConst = abs (productExprConst e2)}))
        k' e'
          | sumExprConst e > 0 = Plus' e' (LitInt' (sumExprConst e))
          | sumExprConst e < 0 = Minus' e' (LitInt' (abs (sumExprConst e)))
          | otherwise = e'
     in k' (foldl go (formatTopProductExpr m eHead) esTail)

-- | `formatTopModuloExpr` convert `ModuloExpr` to `Expr` with adding `FloorMod` at only the root.
formatTopModuloExpr :: ModuloExpr -> Expr
formatTopModuloExpr e = FloorMod' (formatTopSumExpr (modulo e) . unModuloExpr $ normalizeModuloExpr e) (unModulo $ modulo e)

formatBottomInteger :: Modulo -> Integer -> Expr
formatBottomInteger m k = case unModulo m of
  LitInt' m -> LitInt' (k `mod` m)
  m -> FloorMod' (LitInt' k) m

formatBottomProductExpr :: Modulo -> ProductExpr -> Expr
formatBottomProductExpr m e =
  let k = formatBottomInteger m (productExprConst e)
      k' e' = case productExprConst e of
        0 -> LitInt' 0
        1 -> e'
        -1 -> ModNegate' e' (unModulo m)
        _ -> ModMult' e' k (unModulo m)
      invList = map (`ModInv'` unModulo m) (productExprInvList e)
      list = map (\e -> if e `isModulo` m then e else FloorMod' e (unModulo m)) (productExprList e)
   in case list ++ invList of
        [] -> k
        eHead : esTail -> k' (foldl (\e1 e2 -> ModMult' e1 e2 (unModulo m)) eHead esTail)

formatBottomSumExpr :: Modulo -> SumExpr -> Expr
formatBottomSumExpr m e = case sumExprList e of
  [] -> formatBottomInteger m (sumExprConst e)
  eHead : esTail ->
    let go e1 e2
          | productExprConst e2 == 0 = e1
          | productExprConst e2 == -1 = ModMinus' e1 (formatBottomProductExpr m (e2 {productExprConst = 1})) (unModulo m)
          | otherwise = ModPlus' e1 (formatBottomProductExpr m e2) (unModulo m)
        plusConst e'
          | sumExprConst e == 0 = e'
          | otherwise = ModPlus' e' (formatBottomInteger m (sumExprConst e)) (unModulo m)
     in plusConst (foldl go (formatBottomProductExpr m eHead) esTail)

-- | `formatBottomModuloExpr` convert `ModuloExpr` to `Expr` with adding `FloorMod` at every nodes.
formatBottomModuloExpr :: ModuloExpr -> Expr
formatBottomModuloExpr e = formatBottomSumExpr (modulo e) . unModuloExpr $ normalizeModuloExpr e

normalizeProductExpr :: Modulo -> ProductExpr -> ProductExpr
normalizeProductExpr m e =
  let k = case m of
        Modulo (LitInt' m) -> productExprConst e `mod` m
        _ -> productExprConst e
      es =
        if k == 0
          then []
          else sort (productExprList e)
      es' =
        if k == 0
          then []
          else sort (productExprInvList e)
   in e
        { productExprList = es,
          productExprInvList = es',
          productExprConst = k
        }

normalizeSumExpr :: Modulo -> SumExpr -> SumExpr
normalizeSumExpr m e =
  let f e = (productExprList e, productExprInvList e)
      cmp e1 e2 = f e1 `compare` f e2
      cmp' e1 e2 = cmp e1 e2 == EQ
      es = sortBy cmp (map (normalizeProductExpr m) (sumExprList e))
      es' = groupBy cmp' es
      es'' =
        map
          ( \group ->
              ProductExpr
                { productExprConst = sum (map productExprConst group),
                  productExprList = productExprList (head group),
                  productExprInvList = productExprInvList (head group)
                }
          )
          es'
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
arithmeticExprFromModuloExpr e = parseArithmeticExpr . formatTopSumExpr (modulo e) $ unModuloExpr e
