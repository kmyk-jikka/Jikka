{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Jikka.Core.Language.ArithmeticalExpr
  ( -- * Basic functions
    ArithmeticalExpr,
    parseArithmeticalExpr,
    formatArithmeticalExpr,
    integerArithmeticalExpr,
    negateArithmeticalExpr,
    plusArithmeticalExpr,
    minusArithmeticalExpr,
    multArithmeticalExpr,
    isZeroArithmeticalExpr,
    isOneArithmeticalExpr,

    -- * Advanced functions
    unNPlusKPattern,
    makeVectorFromArithmeticalExpr,
    makeAffineFunctionFromArithmeticalExpr,
    splitConstantFactorArithmeticalExpr,
  )
where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List (findIndices, groupBy, sort, sortBy)
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Jikka.Core.Language.BuiltinPatterns
import Jikka.Core.Language.Expr
import Jikka.Core.Language.FreeVars

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

newtype ArithmeticalExpr = ArithmeticalExpr {unArithmeticalExpr :: SumExpr}
  deriving (Show)

instance Eq ArithmeticalExpr where
  e1 == e2 = unArithmeticalExpr (normalizeArithmeticalExpr e1) == unArithmeticalExpr (normalizeArithmeticalExpr e2)

instance Ord ArithmeticalExpr where
  e1 `compare` e2 = unArithmeticalExpr (normalizeArithmeticalExpr e1) `compare` unArithmeticalExpr (normalizeArithmeticalExpr e2)

integerProductExpr :: Integer -> ProductExpr
integerProductExpr n =
  ProductExpr
    { productExprConst = n,
      productExprList = []
    }

negateProductExpr :: ProductExpr -> ProductExpr
negateProductExpr e = e {productExprConst = negate (productExprConst e)}

multProductExpr :: ProductExpr -> ProductExpr -> ProductExpr
multProductExpr e1 e2 =
  ProductExpr
    { productExprConst = productExprConst e1 * productExprConst e2,
      productExprList = productExprList e1 ++ productExprList e2
    }

iterateN :: Integer -> (a -> a) -> a -> a
iterateN n _ _ | n < 0 = error $ "iterateN: negative number: " ++ show n
iterateN 0 _ x = x
iterateN n f x = iterateN (n - 1) f (f x)

parseProductExpr :: Expr -> ProductExpr
parseProductExpr = \case
  LitInt' n -> ProductExpr {productExprConst = n, productExprList = []}
  Negate' e -> negateProductExpr (parseProductExpr e)
  Mult' e1 e2 -> multProductExpr (parseProductExpr e1) (parseProductExpr e2)
  Pow' e1 (LitInt' k) | 0 <= k && k < 10 -> iterateN k (multProductExpr (parseProductExpr e1)) (integerProductExpr 1)
  e -> ProductExpr {productExprConst = 1, productExprList = [e]}

sumExprFromProductExpr :: ProductExpr -> SumExpr
sumExprFromProductExpr e =
  SumExpr
    { sumExprList = [e],
      sumExprConst = 0
    }

arithmeticalExprFromProductExpr :: ProductExpr -> ArithmeticalExpr
arithmeticalExprFromProductExpr = ArithmeticalExpr . sumExprFromProductExpr

integerSumExpr :: Integer -> SumExpr
integerSumExpr n =
  SumExpr
    { sumExprConst = n,
      sumExprList = []
    }

integerArithmeticalExpr :: Integer -> ArithmeticalExpr
integerArithmeticalExpr = ArithmeticalExpr . integerSumExpr

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

multSumExpr :: SumExpr -> SumExpr -> SumExpr
multSumExpr e1 e2 =
  SumExpr
    { sumExprList =
        let es1 = parseProductExpr (LitInt' (sumExprConst e1)) : sumExprList e1
            es2 = parseProductExpr (LitInt' (sumExprConst e2)) : sumExprList e2
         in tail $ map (uncurry multProductExpr) ((,) <$> es1 <*> es2),
      sumExprConst = sumExprConst e1 * sumExprConst e2
    }

negateArithmeticalExpr :: ArithmeticalExpr -> ArithmeticalExpr
negateArithmeticalExpr (ArithmeticalExpr e) = ArithmeticalExpr $ negateSumExpr e

plusArithmeticalExpr :: ArithmeticalExpr -> ArithmeticalExpr -> ArithmeticalExpr
plusArithmeticalExpr (ArithmeticalExpr e1) (ArithmeticalExpr e2) = ArithmeticalExpr $ plusSumExpr e1 e2

minusArithmeticalExpr :: ArithmeticalExpr -> ArithmeticalExpr -> ArithmeticalExpr
minusArithmeticalExpr (ArithmeticalExpr e1) (ArithmeticalExpr e2) = ArithmeticalExpr $ plusSumExpr e1 (negateSumExpr e2)

multArithmeticalExpr :: ArithmeticalExpr -> ArithmeticalExpr -> ArithmeticalExpr
multArithmeticalExpr (ArithmeticalExpr e1) (ArithmeticalExpr e2) = ArithmeticalExpr $ multSumExpr e1 e2

parseSumExpr :: Expr -> SumExpr
parseSumExpr = \case
  LitInt' n -> SumExpr {sumExprList = [], sumExprConst = n}
  Negate' e -> negateSumExpr (parseSumExpr e)
  Plus' e1 e2 -> plusSumExpr (parseSumExpr e1) (parseSumExpr e2)
  Minus' e1 e2 -> plusSumExpr (parseSumExpr e1) (negateSumExpr (parseSumExpr e2))
  Mult' e1 e2 -> multSumExpr (parseSumExpr e1) (parseSumExpr e2)
  e -> sumExprFromProductExpr (parseProductExpr e)

-- | `parseArithmeticalExpr` converts a given expr to a normal form \(\sum_i \prod_j e _ {i,j})\).
-- This assumes given exprs have the type \(\mathbf{int}\).
parseArithmeticalExpr :: Expr -> ArithmeticalExpr
parseArithmeticalExpr = ArithmeticalExpr . parseSumExpr

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

formatArithmeticalExpr :: ArithmeticalExpr -> Expr
formatArithmeticalExpr = formatSumExpr . unArithmeticalExpr . normalizeArithmeticalExpr

normalizeProductExpr :: ProductExpr -> ProductExpr
normalizeProductExpr e =
  let es =
        if productExprConst e == 0
          then []
          else sort (productExprList e)
   in e {productExprList = es}

normalizeSumExpr :: SumExpr -> SumExpr
normalizeSumExpr e =
  let cmp e1 e2 = productExprList e1 `compare` productExprList e2
      cmp' e1 e2 = cmp e1 e2 == EQ
      es = sortBy cmp (map normalizeProductExpr (sumExprList e))
      es' = groupBy cmp' es
      es'' = map (\group -> ProductExpr {productExprConst = sum (map productExprConst group), productExprList = productExprList (head group)}) es'
      es''' = filter (\e -> productExprConst e /= 0 && not (null (productExprList e))) es''
      k = sum (map (\e -> if null (productExprList e) then productExprConst e else 0) es'')
   in SumExpr
        { sumExprList = es''',
          sumExprConst = sumExprConst e + k
        }

normalizeArithmeticalExpr :: ArithmeticalExpr -> ArithmeticalExpr
normalizeArithmeticalExpr = ArithmeticalExpr . normalizeSumExpr . unArithmeticalExpr

-- | `makeVectorFromArithmeticalExpr` makes a vector \(f\) and a expr \(c\) from a given vector of variables \(x_0, x_1, \dots, x _ {n - 1}\) and a given expr \(e\) s.t. \(f\) and \(c\) don't have \(x_0, x_1, \dots, x _ {n - 1}\) as free variables and \(e = c + f \cdot (x_0, x_1, \dots, x _ {n - 1})\) holds.
-- This assumes given variables and exprs have the type \(\mathbf{int}\).
--
-- * The returned exprs are normalized with `normalizeArithmeticalExpr`.
makeVectorFromArithmeticalExpr :: V.Vector VarName -> ArithmeticalExpr -> Maybe (V.Vector ArithmeticalExpr, ArithmeticalExpr)
makeVectorFromArithmeticalExpr xs es = runST $ do
  runMaybeT $ do
    f <- lift $ MV.replicate (V.length xs) (integerArithmeticalExpr 0)
    c <- lift $ newSTRef (integerArithmeticalExpr (sumExprConst (unArithmeticalExpr es)))
    forM_ (sumExprList (unArithmeticalExpr es)) $ \e -> do
      let indices = V.imap (\i x -> map (i,) (findIndices (x `isFreeVar`) (productExprList e))) xs
      case concat (V.toList indices) of
        [] -> lift $ modifySTRef c (plusArithmeticalExpr (arithmeticalExprFromProductExpr e))
        [(i, j)] -> do
          let e' = e {productExprList = take j (productExprList e) ++ drop (j + 1) (productExprList e)}
          lift $ MV.modify f (plusArithmeticalExpr (arithmeticalExprFromProductExpr e')) i
        _ -> MaybeT $ return Nothing
    f <- V.freeze f
    c <- lift $ readSTRef c
    return (V.map normalizeArithmeticalExpr f, normalizeArithmeticalExpr c)

isZeroArithmeticalExpr :: ArithmeticalExpr -> Bool
isZeroArithmeticalExpr e = normalizeArithmeticalExpr e == integerArithmeticalExpr 0

isOneArithmeticalExpr :: ArithmeticalExpr -> Bool
isOneArithmeticalExpr e = normalizeArithmeticalExpr e == integerArithmeticalExpr 1

-- | `unNPlusKPattern` recognizes a pattern of \(x + k\) for a variable \(x\) and an integer constant \(k \in \mathbb{Z}\).
unNPlusKPattern :: ArithmeticalExpr -> Maybe (VarName, Integer)
unNPlusKPattern e = case normalizeArithmeticalExpr e of
  ArithmeticalExpr
    SumExpr
      { sumExprList =
          [ ProductExpr
              { productExprConst = 1,
                productExprList = [Var x]
              }
            ],
        sumExprConst = k
      } -> Just (x, k)
  _ -> Nothing

-- | `makeAffineFunctionFromArithmeticalExpr` is a specialized version of `makeVectorFromArithmeticalExpr`.
-- This function returns \(a, b\) for a given variable \(x\) and a given expr \(e = a x + b\) where \(a, b\) which doesn't use \(x\) free.
makeAffineFunctionFromArithmeticalExpr :: VarName -> ArithmeticalExpr -> Maybe (ArithmeticalExpr, ArithmeticalExpr)
makeAffineFunctionFromArithmeticalExpr x es = first V.head <$> makeVectorFromArithmeticalExpr (V.singleton x) es

-- | `splitConstantFactorArithmeticalExpr` finds \(k\) and \(e'\) for given \(e\) s.t. \(e = k e'\).
splitConstantFactorArithmeticalExpr :: ArithmeticalExpr -> (Integer, ArithmeticalExpr)
splitConstantFactorArithmeticalExpr e =
  let e' = unArithmeticalExpr $ normalizeArithmeticalExpr e
   in case (sumExprConst e', sumExprList e') of
        (0, []) -> (0, integerArithmeticalExpr 0)
        (k, []) -> (k, integerArithmeticalExpr 1)
        (0, [e]) -> second arithmeticalExprFromProductExpr $ splitConstantFactorProductExpr e
        (k, es) ->
          let kes = map splitConstantFactorProductExpr es
              d = foldl gcd k (map fst kes)
           in ( d,
                ArithmeticalExpr
                  SumExpr
                    { sumExprConst = k `div` d,
                      sumExprList = map (\(k, e) -> e {productExprConst = (k * productExprConst e) `div` d}) kes
                    }
              )

splitConstantFactorProductExpr :: ProductExpr -> (Integer, ProductExpr)
splitConstantFactorProductExpr e = (productExprConst e, e {productExprConst = 1})
